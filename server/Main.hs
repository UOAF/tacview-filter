module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.IORef
import Data.Tacview.Source qualified as Tacview
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
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
            progDesc "Replays ACMI files at the rate they were recorded"
    parser >>= run

run :: Args -> IO ()
run Args{..} = do
    linesRead <- newIORef 0
    let _src = Tacview.source zipInput linesRead
    server serverName port

stderrLock :: MVar ()
stderrLock = unsafePerformIO $ newMVar ()
{-# NOINLINE stderrLock #-}

-- TODO: Actual contravariant tracing?
slog :: String -> IO ()
slog s = withMVar stderrLock . const $ do
    hPutStrLn stderr s

server :: Text -> Word16 -> IO ()
server serverName port = do
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
            forkFinally (serve serverName sock addr) (const $ cleanup sock addr)

serve :: Text -> Socket -> SockAddr -> IO ()
serve serverName sock who = do
    doHandshake serverName sock who

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

