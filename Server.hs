module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Channel
import Control.Concurrent.STM
import Control.Concurrent.TBCQueue
import Control.Exception.Safe
import Control.Monad
import Data.ByteString qualified as BS
import Data.Function (fix)
import Data.IORef
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Tacview
import Data.Tacview.Delta
import Data.Tacview.Ignores as Ignores
import Data.Tacview.Source qualified as Tacview
import Data.Tacview.Sink qualified as Tacview
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word
import Foreign.StablePtr
import Network.Socket
import Network.Socket.ByteString qualified as NBS
import Options.Applicative
import System.IO
import System.IO.Unsafe
import System.Timeout

data Args = Args {
    zipOutput :: Maybe FilePath,
    port :: Maybe Word16,
    serverName :: Text,
    zipInput :: Maybe FilePath
}

parseArgs :: Parser Args
parseArgs = Args <$> parseZipOut <*> parsePort <*> parseName <*> parseZipIn where
    parseZipIn = optional . strArgument $ mconcat [
        help "ACMI to serve. Otherwise reads from stdin",
        metavar "recording.zip.acmi"
        ]
    parseZipOut = optional . strOption $ mconcat [
        short 'o',
        long "output",
        help "Path to save a copy of the ACMI",
        metavar "out.zip.acmi"
        ]
    parsePort = optional . option auto $ mconcat [
        short 'p',
        long "port",
        help "Serve ACMI on this port (Tacview default is 42674)"
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

    -- lol IO
    hSetBuffering stderr LineBuffering
    hSetBuffering stdin $ BlockBuffering Nothing
    hSetBuffering stdout $ BlockBuffering Nothing

    let parser = customExecParser (prefs showHelpOnError) parseInfo
        parseInfo = info (parseArgs <**> helper) $
            progDesc "Serves and filters ACMI"
    parser >>= run

data ServerState = ServerState {
    fileStream :: Maybe (TBCQueue Text),
    -- | Connected clients, represented as a map of channels to write to,
    --   addressed by the client address (mostly so they can easily deregister themselves)
    clients :: TVar (Map SockAddr (TBCQueue Text)),
    -- | "Global" `0,...` lines that should be sent as a prologue to each client as they connect
    globalLines :: TVar (Vector Text),
    -- | Objects that should be sent to clients (after global lines) as they connect
    --   to catch them up.
    liveObjects :: TVar (HashMap TacId ObjectState),
    -- If we're going to make the above mutable in IO,
    -- we might as well do the same for these,
    -- instead of mutating some and tail-calling the others.
    -- IORef is fine since the state is isolated to one thread.
    now :: IORef Double,
    lastWrittenTime :: IORef Double
}

withServerState :: Maybe FilePath -> (ServerState -> IO a) -> IO a
withServerState tacOut f = do
    clients <- newTVarIO mempty
    globalLines <- newTVarIO mempty
    liveObjects <- newTVarIO mempty
    now <- newIORef 0
    lastWrittenTime <- newIORef 0
    -- Could probably benefit from some ContT love, but meanwhile,
    case tacOut of
        Nothing -> do
            let fileStream = Nothing
            f ServerState{..}
        Just to -> withFileStream to $ \fs -> do
            let fileStream = Just fs
            f ServerState{..}

withFileStream :: FilePath -> (TBCQueue Text -> IO a) -> IO a
withFileStream tacOut f = do
    let go q = runSink q `catchIO` handler
        runSink q = Tacview.sinkZip tacOut >>= ($ q)
        handler e = slog $ "Error writing to " <> tacOut <> ": " <> show e
    fst <$> pipeline (newTBCQueueIO 1024) f go

run :: Args -> IO ()
run Args{..} = withServerState zipOutput $ \ss -> do
    (src, _, _) <- Tacview.source zipInput
    let pipe = pipeline (newTBCQueueIO 1024)
        ignore sink = pipe src (`filterLines` sink)
        piped = pipe ignore (feed ss)
    case port of
        Just p -> race_ piped (server ss serverName p)
        Nothing -> void piped

    -- Close all clients (now that we're not spawning any more)...
    atomically $ do
        mapM_ closeChannel ss.fileStream
        clients <- readTVar ss.clients
        mapM_ closeChannel clients

    -- ...and wait for them to finish.
    atomically $ do
        clients <- readTVar ss.clients
        check $ M.null clients

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
    -- This might start to fail if the output file stream dies.
    forM_ ss.fileStream $ \fs -> mapM_ (atomically . writeChannel fs) ts
    -- Keep feeding network clients even if so.
    (fulls, readies) <- atomically $ do
        -- I want some partitionM - listing, zipping, and unlisting is dumb.
        clients <- M.toList <$> readTVar ss.clients
        fullStatuses <- mapM isFullChannel $ snd <$> clients
        let clientFullPairs = zip clients fullStatuses
            (f, r) = partition snd clientFullPairs
            (f', r') = (M.fromList $ fst <$> f, M.fromList $ fst <$> r)
        -- Cull full channels
        mapM_ closeChannel f'
        writeTVar ss.clients r'
        pure (f', r')

    forM_ (M.keys fulls) $ \k -> slog $ show k <> " dropped for not keeping up"
    forM_ (M.elems readies) $ \c -> mapM_ (atomically . tryWriteChannel c) ts

feed :: Channel c => ServerState -> c ParsedLine -> IO ()
feed ss source = do
    consumeChannel source $ \p -> do
        newLines <- feed' ss p
        writeToClients ss $ catMaybes newLines

    -- For each client, close out each remaining object.
    liveObjects <- readTVarIO ss.liveObjects
    let closeLine :: TacId -> ObjectState -> Maybe ParsedLine
        closeLine i s = PropLine i <$> closeOut s
        allClosed = mapMaybe (uncurry closeLine) (HM.toList liveObjects)
    writeToClients ss allClosed

-- A very similar shape as filter's `processLine`, but
-- 1. Keeps track of all global lines so newly-connected clients can get those.
-- 2. Makes live objects atomic so newly-connected clients can those /as/ we update that here.
feed' :: ServerState -> ParsedLine -> IO [Maybe ParsedLine]
feed' ss p = let
    -- A timestamp, when we need a new one.
    writeTimestamp :: IO (Maybe ParsedLine)
    writeTimestamp = do
        now <- readIORef ss.now
        lastTime <- readIORef ss.lastWrittenTime
        if now /= lastTime
            then do
                writeIORef ss.lastWrittenTime now
                pure . Just $ TimeLine now
            else pure Nothing

    -- Helper to remove the object from the set we're tracking, taking the ID
    axeIt :: TacId -> IO [Maybe ParsedLine]
    axeIt x = do
        maybeTime <- writeTimestamp
        -- We might not have written in in a bit,
        -- so make sure its last known state goes out.
        co <- atomically $ do
            lo <- readTVar ss.liveObjects
            case lo HM.!? x of
                Just going -> do
                    writeTVar ss.liveObjects $ HM.delete x lo
                    pure $ closeOut going
                Nothing -> pure Nothing
        pure [PropLine x <$> co, maybeTime, Just p]

    -- Helper to pass the line through without doing anything, taking the time.
    passthrough :: IO [Maybe ParsedLine]
    passthrough = do
        maybeTime <- writeTimestamp
        pure [maybeTime, Just p]

    in case p of
        PropLine pid props -> do
            now <- readIORef ss.now
            -- Is it anything we're tracking?
            deltaLine <- atomically $ do
                lo <- readTVar ss.liveObjects
                let prev = lo HM.!? pid
                -- Update the properties we're tracking.
                let (newState, dl) = updateObject prev now props
                writeTVar ss.liveObjects $ HM.insert pid newState lo
                pure dl
            -- If we have a delta line, write some stuff...
            case deltaLine of
                Just dl -> do
                    maybeTime <- writeTimestamp
                    pure [maybeTime, Just $ PropLine pid dl]
                Nothing -> pure [] -- Otherwise just update liveObjects
        RemLine pid -> axeIt pid
        -- If it's a "left the area" event, assume its deletion will
        -- come next. We want to write out any last properties before going.
        EventLine t es _ -> if t == "LeftArea"
            then assert (S.size es == 1) $ do
                axeIt (head $ S.toList es)
            else passthrough
        -- If it's a #<time> line, note the new time but don't write.
        TimeLine t -> do
            writeIORef ss.now t
            pure []
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
        forever $ do
            (sock, addr) <- accept lsock
            async' (serve ss serverName sock addr) >>= link

async' :: IO a -> IO (Async a)
async' f = async $ do
    -- https://well-typed.com/blog/2024/01/when-blocked-indefinitely-is-not-indefinite/
    mytid <- myThreadId
    bracket (newStablePtr mytid) freeStablePtr (const f)

serve :: ServerState -> Text -> Socket -> SockAddr -> IO ()
serve ss serverName sock who = c `finally` close sock where
    c = serve' ss serverName sock who `catchIO` handler
    handler e = slog $ show who <> " hung up: " <> show e

serve' :: ServerState -> Text -> Socket -> SockAddr -> IO ()
serve' ss serverName sock who = do
    doHandshake serverName sock who
    chan <- newTBCQueueIO 1024
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
            loLines = (\(k, v) -> PropLine k v.osCurrent) <$> HM.toList lo
            syncBytes = T.encodeUtf8 $ T.unlines $ glLines <> fmap showLine loLines
        NBS.sendAll sock syncBytes
        slog $ "SYNCED " <> show who <> " (" <> show (BS.length syncBytes) <> " bytes)"

        -- Don't send one line at a time if we can help it;
        -- Send everything that's been queued.
        fix $ \loop -> atomically (drainChannel chan) >>= \case
                [] -> slog $ "DROP " <> show who
                drainedLines -> do
                    NBS.sendAll sock . T.encodeUtf8 . T.unlines $ drainedLines
                    loop

doHandshake :: Text -> Socket -> SockAddr ->  IO ()
doHandshake tname sock who = do
    slog $ show who <> " handshake"
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

