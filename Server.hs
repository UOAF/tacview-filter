module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Channel
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.Function (fix)
import Data.IORef
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Tacview
import Data.Tacview.Delta
import Data.Tacview.Ignores as Ignores
import Data.Tacview.Source qualified as Tacview
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word
import Network.Socket
import Network.Socket.ByteString qualified as NBS
import Options.Applicative
import System.IO
import System.IO.Unsafe
import System.Timeout

data Args = Args {
    zipInput :: Maybe FilePath,
    port :: Word16,
    serverName :: Text
}

parseArgs :: Parser Args
parseArgs = Args <$> parseZipIn <*> parsePort <*> parseName where
    parseZipIn = optional . strArgument $ mconcat [
        help "ACMI to serve. Otherwise reads from stdin",
        metavar "recording.zip.acmi"
        ]
    parsePort = option auto $ mconcat [
        short 'p',
        long "port",
        help "Server port",
        value 42674
        ]
    parseName = fmap T.pack . strOption $ mconcat [
        short 'n',
        long "server-name",
        help "Server name shown to Tacview clients",
        value "tacview-server"
        ]

main :: IO ()
main = do
    -- lol Windows
    hSetNewlineMode stdin noNewlineTranslation
    hSetNewlineMode stdout noNewlineTranslation
    hSetNewlineMode stderr noNewlineTranslation

    let parser = customExecParser (prefs showHelpOnError) parseInfo
        parseInfo = info (parseArgs <**> helper) $
            progDesc "Serves and filters ACMI"
    parser >>= run

data ServerState = ServerState {
    clients :: TVar (Map SockAddr (Channel Text)),
    globalLines :: TVar (Vector Text),
    liveObjects :: TVar (HashMap TacId ObjectState),
    now :: Double,
    lastWrittenTime :: Double
}

run :: Args -> IO ()
run Args{..} = do
    linesRead <- newIORef 0
    ss <- ServerState <$>
        newTVarIO mempty <*> newTVarIO mempty <*> newTVarIO mempty <*> pure 0 <*> pure 0
    let src = Tacview.source zipInput linesRead
        ignore sink = pipeline
            src
            (\source -> filterLines Ignores.startState source sink)
        piped = pipeline ignore (feed ss)
    race_ piped (server ss serverName port)

    -- Wait for all clients?
    -- This fails with blocked indefinitely because
    -- https://well-typed.com/blog/2024/01/when-blocked-indefinitely-is-not-indefinite/
    {-
    NO atomically $ do
    NO  clients <- readTVar ss.clients
    NO  check $ M.null clients
    -}

stderrLock :: MVar ()
stderrLock = unsafePerformIO $ newMVar ()
{-# NOINLINE stderrLock #-}

-- TODO: Actual contravariant tracing?
slog :: String -> IO ()
slog s = withMVar stderrLock . const $ do
    hPutStrLn stderr s

-- | Write out to each client.
--
-- TODO: Use tryWriteChannel and drop guys who can't keep up.
writeToClients :: ServerState -> [ParsedLine] -> IO ()
writeToClients ss pl = do
    let ts = showLine <$> pl
    chans <- M.elems <$> readTVarIO ss.clients
    forM_ chans $ \c -> mapM_ (evalWriteChannel c) ts

feed :: ServerState -> Channel ParsedLine -> IO ()
feed ss source = atomically (readChannel source) >>= \case
     Nothing -> do
        -- For each client, close out each remaining object.
        liveObjects <- readTVarIO ss.liveObjects
        let closeLine :: TacId -> ObjectState -> Maybe ParsedLine
            closeLine i s = PropLine i <$> closeOut s
            allClosed = catMaybes $ uncurry closeLine <$> HM.toList liveObjects
        writeToClients ss allClosed
     Just p -> do
        (newLines, newState) <- feed' ss p
        writeToClients newState $ catMaybes newLines
        feed newState source

feed' :: ServerState -> ParsedLine -> IO ([Maybe ParsedLine], ServerState)
feed' !ss p = let
    -- A timestamp, when we need a new one.
    writeTimestamp :: Maybe ParsedLine
    writeTimestamp = if (ss.now /= ss.lastWrittenTime)
        then Just $ TimeLine ss.now
        else Nothing
    -- Helper to remove the object from the set we're tracking, taking the ID
    axeIt :: TacId -> IO ([Maybe ParsedLine], ServerState)
    axeIt x = do
        -- We might not have written in in a bit,
        -- so make sure its last known state goes out.
        co <- atomically $ do
            lo <- readTVar ss.liveObjects
            case lo HM.!? x of
                Just going -> do
                    writeTVar ss.liveObjects $ HM.delete x lo
                    pure $ closeOut going
                Nothing -> pure Nothing

        let newState = ss { lastWrittenTime = ss.now }
        pure ([PropLine x <$> co, writeTimestamp, Just p], newState)
    -- Helper to pass the line through without doing anything, taking the time.
    passthrough :: IO ([Maybe ParsedLine], ServerState)
    passthrough = let
        newState = ss { lastWrittenTime = ss.now }
        in pure ([writeTimestamp, Just p], newState)
    in case p of
        PropLine pid props -> do
            -- Is it anything we're tracking?
            deltaLine <- atomically $ do
                lo <- readTVar ss.liveObjects
                let prev = lo HM.!? pid
                -- Update the properties we're tracking.
                let (newState, dl) = updateObject prev ss.now props
                writeTVar ss.liveObjects $ HM.insert pid newState lo
                pure dl
            -- If we have a delta line, write some stuff...
            case deltaLine of
                Just dl -> pure (
                    [writeTimestamp, Just $ PropLine pid dl],
                    ss { lastWrittenTime = ss.now }
                    )
                Nothing -> pure ([], ss) -- Otherwise just update liveObjects
        RemLine pid -> axeIt pid
        -- If it's a "left the area" event, assume its deletion will
        -- come next. We want to write out any last properties before going.
        EventLine es l -> if T.isInfixOf "Event=LeftArea" l
            then assert (HS.size es == 1) $ do
                axeIt (head $ HS.toList es)
            else passthrough
        -- If it's a #<time> line, note the new time but don't write.
        TimeLine t -> pure ([], ss { now = t })
        -- Assume some global event or something that every connecting client should get.
        OtherLine l -> do
            atomically $ modifyTVar' ss.globalLines $ flip V.snoc l
            passthrough

server :: ServerState -> Text -> Word16 -> IO ()
server ss serverName port = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
    bindAddr <- head <$> getAddrInfo (Just hints) Nothing (Just $ show port)
    bracket (openSocket bindAddr) close $ \lsock -> do
        setSocketOption lsock ReuseAddr 1
        bind lsock $ addrAddress bindAddr
        listen lsock 32
        let cleanup sock addr = do
                close sock
                hPutStrLn stderr $ show addr <> " hung up"
        forever $ do
            (sock, addr) <- accept lsock
            forkFinally (serve ss serverName sock addr) (const $ cleanup sock addr)

serve :: ServerState -> Text -> Socket -> SockAddr -> IO ()
serve ss serverName sock who = do
    doHandshake serverName sock who
    chan <- newChannelIO 1024
    let register = atomically $ do
            -- It's important we atomically add ourselves *and* get the current state of the world.
            -- This guarantees we're synced up with everyone and ready for the next update
            -- to arrive on our channel.
            modifyTVar' ss.clients $ M.insert who chan
            gl <- readTVar ss.globalLines
            lo <- readTVar ss.liveObjects
            pure (gl, lo)
        deregister _ = atomically $ modifyTVar' ss.clients $ M.delete who
    bracket register deregister $ \(gl, lo) -> do
        -- Send the current state of the world at the time of connection.
        let glLines = V.toList gl
            loLines = fmap (\(k, v) -> PropLine k v.osCurrent) $ HM.toList lo
        NBS.sendAll sock . T.encodeUtf8 . T.unlines $ glLines <> fmap showLine loLines

        -- TODO: Drain channel and send as a chunk.
        fix $ \loop -> atomically (readChannel chan) >>= \case
            Nothing -> pure ()
            Just l -> do
                NBS.sendAll sock . T.encodeUtf8 $ l <> "\n"
                loop

doHandshake :: Text -> Socket -> SockAddr ->  IO ()
doHandshake tname sock who = do
    slog $ "Shaking hands with " <> show who
    let sh = serverHandshake tname
    NBS.sendAll sock sh
    mresponse <- timeout (round 5e6) $ parseClientHandshake <$> rxClientHandshake sock ""
    case mresponse of
        Nothing -> slog $ show who <> " timed out"
        Just (Left e) -> slog $ show who <> " had bad handshake:\n" <> T.unpack e
        Just (Right c) -> do
            slog $ show who <> " connected as " <> T.unpack c


serverHandshake :: Text -> BS.ByteString
serverHandshake tname = T.encodeUtf8 $
    "XtraLib.Stream.0\n" <>
    "Tacview.RealTimeTelemetry.0\n" <>
    tname <> "\n" <>
    "0\0"

-- Receive until we have four lines
rxClientHandshake :: Socket -> BS.ByteString -> IO Text
rxClientHandshake sock acc = do
    -- Just reparse every time lol - it's four measley lines
    let t = T.decodeUtf8Lenient acc
    if length (T.lines t) >= 4
        then pure t
        else do
            moar <- NBS.recv sock 2048 -- Don't need lazy byte string, we'll do this once or twice.
            rxClientHandshake sock $ acc <> moar

parseClientHandshake :: Text -> Either Text Text
parseClientHandshake cs
    | "XtraLib.Stream.0\nTacview.RealTimeTelemetry.0\n" `T.isPrefixOf` cs &&
      "\n0\0" `T.isSuffixOf` cs = Right $ T.lines cs !! 2
    | otherwise = Left cs

