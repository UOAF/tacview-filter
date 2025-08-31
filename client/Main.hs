{-# LANGUAGE OverloadedStrings #-}
import Control.Exception
import Data.ByteString qualified as BS
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lazy qualified as BSL
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Network.Socket
import Network.Socket.ByteString qualified as NBS
import System.IO

main :: IO ()
main = do
    hSetBuffering stderr $ LineBuffering
    hSetBuffering stdin $ BlockBuffering Nothing
    hSetBuffering stdout $ BlockBuffering Nothing

    let hints = defaultHints { addrFamily = AF_UNSPEC, addrSocketType = Stream }
    addr <- head <$> getAddrInfo (Just hints) (Just "127.0.0.1" ) (Just "42674")
    bracket (openSocket addr) close $ \sock -> do
        connect sock (addrAddress addr)
        -- Handshake:
        -- Should do this in a loop since we might not get the whole handshake
        -- in a single recv, but it tends to Just Work™.
        buf <- NBS.recv sock 2048
        let t = T.decodeUtf8 buf
        T.hPutStrLn stderr t -- Debug - print the server handshake
        NBS.sendAll sock handshakeResponse -- Respond without validating lol.
        readLoop sock "" -- Start streamin'

handshakeResponse :: BS.ByteString
handshakeResponse = T.encodeUtf8 $
    "XtraLib.Stream.0\n" <>
    "Tacview.RealTimeTelemetry.0\n" <>
    "Test Client Please Ignore\n" <>
    "0\0"

-- Some futzing to rejoin lines cut in two by our reads
readLoop :: Socket -> BS.ByteString -> IO ()
readLoop sock prev = do
    moar <- NBS.recv sock 4096 -- Get some Tacview goodness from the socket
    if BS.null moar
        then handleBuf (BS.fromStrict prev) -- Server hung up
        else do -- Keep chugging
            -- Find the last newline - everything before that is a whole line.
            -- There's a good chance our read stopped in the middle of the last one,
            -- so it might be partial - hang onto that and reuse it next time.
            let (wholeLines, partialLastLine) = case BS.findIndexEnd (== c2w '\n') moar of
                    Just i -> BS.splitAt i moar
                    Nothing -> (moar, "")
            -- Join any leftover bytes from last time and the complete lines from this time.
            handleBuf (BSL.fromChunks [prev, wholeLines])
            readLoop sock partialLastLine

-- Handle each chunk of lines
handleBuf :: BSL.ByteString -> IO ()
handleBuf b = do
    -- Decode to UTF-8 and throw out the blank ones
    let bufLines = filter (not . TL.null) . TL.lines $ TL.decodeUtf8 b
    -- TODO: Do something with the decoded lines. For now we'll just print them.
    mapM_ TL.putStrLn bufLines
