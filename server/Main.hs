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
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Tacview
import Data.Tacview.Ignores as Ignores
import Data.Tacview.Source qualified as Tacview
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
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
    globalLines :: TVar (Vector Text)
}

run :: Args -> IO ()
run Args{..} = do
    linesRead <- newIORef 0
    ss <- ServerState <$> newTVarIO mempty <*> newTVarIO mempty
    let src = Tacview.source zipInput linesRead
        ignore sink = pipeline
            src
            (\source -> filterLines Ignores.startState source sink)
        piped = pipeline ignore (feed ss)
    concurrently_ piped (server ss serverName port)

stderrLock :: MVar ()
stderrLock = unsafePerformIO $ newMVar ()
{-# NOINLINE stderrLock #-}

-- TODO: Actual contravariant tracing?
slog :: String -> IO ()
slog s = withMVar stderrLock . const $ do
    hPutStrLn stderr s

feed :: ServerState -> Channel (ParsedLine, Text) -> IO ()
feed ss source = fix $ \loop -> atomically (readChannel source) >>= \case
     Nothing -> pure ()
     Just (p, l) -> do
        feed' ss p l
        loop

feed' :: ServerState -> ParsedLine -> Text -> IO ()
feed' ServerState{..} p l = do
    case p of
        -- Assume some global event or something that every connecting client should get.
        GlobalLine -> atomically $ modifyTVar' globalLines $ flip V.snoc l
        -- Write through for now
        _ -> atomically $ do
            cs <- readTVar clients
            mapM_ (`writeChannel` l) cs

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
serve ServerState{..} serverName sock who = do
    doHandshake serverName sock who
    chan <- newChannelIO 1024
    let register = atomically $ do
            modifyTVar' clients $ M.insert who chan
            readTVar globalLines
        deregister _ = atomically $ modifyTVar' clients $ M.delete who
    bracket register deregister $ \startingLines -> do
        -- Send the current state of the world at the time of connection
        -- TODO ALL THE THINGS WE'RE TRACKING TOO
        let sl = T.unlines $ V.toList startingLines
        NBS.sendAll sock . T.encodeUtf8 $ sl

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

