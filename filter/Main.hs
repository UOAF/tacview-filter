{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Main where

import Codec.Archive.Zip
import Codec.Archive.Zip.Internal qualified as ZI
import Control.Concurrent.Async
import Control.Concurrent hiding (yield)
import Control.Concurrent.Channel
import Control.Concurrent.STM
import Control.Monad
import Conduit
import Data.ByteString qualified as BS
import Data.Function (fix)
import Data.IORef
import Data.List
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.Float
import Options.Applicative
import System.Clock.Seconds
import System.Console.ANSI
import System.IO
import Text.Printf

import Delta
import Ignores

data Args = Args {
    zipInput :: Maybe FilePath,
    noProgress :: Bool
}

parseArgs :: Parser Args
parseArgs = Args <$> parseZipIn <*> parseNoProgress where
    parseZipIn = optional . strArgument $ mconcat [
        help "Zipped ACMI to filter. Otherwise reads from stdin and writes to stdout",
        metavar "recording.zip.acmi"
        ]
    parseNoProgress = switch $ mconcat [
        long "no-progress",
        help "Suppress progress printed to stderr"
        ]


main :: IO ()
main = do
    -- lol Windows
    hSetNewlineMode stdin noNewlineTranslation
    hSetNewlineMode stdout noNewlineTranslation
    hSetNewlineMode stderr noNewlineTranslation

    let parser = customExecParser (prefs showHelpOnError) parseInfo
        parseInfo = info (parseArgs <**> helper) $
            progDesc "Ensmallens ACMI files"
    parser >>= runFilter

runFilter :: Args -> IO ()
runFilter Args{..} = do
    start <- getTime Monotonic

    -- Hoare Was Right.
    -- The program is pipelined (see Data.Concurrency.Channel):
    -- Major steps are run in their own task.
    -- They filters or modify lines, then pass the remainders to the next stage of the pipe.
    -- This parallelizes trivially; the runtime runs each task in a free thread.
    src <- maybe (pure readStdin) readZip zipInput
        :: IO (ConduitT () BS.ByteString (ResourceT IO) ())
    linesRead <- newIORef 0
    linesWritten <- newIORef 0
    let filterInput = feed src linesRead
        dest = maybe (writeStdout) writeZip zipInput
        filterOutput = write linesWritten dest
        ignore sink = pipeline filterInput (\source -> filterLines Ignores.startState source sink)
        thenDeltas sink = pipeline ignore (\source -> deltas Delta.startState source sink)
        thenDropTimes = pipeline thenDeltas filterOutput
        prog = if noProgress
            then forever $ threadDelay maxBound
            else progress linesRead linesWritten

    runnit <- race thenDropTimes prog

    -- Gather up all our stats, placed in newtypes for easier readability here.
    let (((InputLines i, FilteredLines f), DecimatedLines d), OutputLines o) = case runnit of
            Left l -> l
            Right () -> error "absurd: progress should run forever"

    end <- getTime Monotonic
    let dt = end - start
        dts = printf "%.2f" (realToFrac dt :: Double)
    unless noProgress $ do
        -- Clear any progress bar
        hClearFromCursorToLineBeginning stderr
        hCursorDownLine stderr 0
    hPutStrLn stderr $ "in " <> dts <> " seconds"
    hPutStrLn stderr $ show i <> " lines read"
    hPutStrLn stderr $ show f <> " lines ignored"
    hPutStrLn stderr $ show d <> " lines decimated"
    -- hPutStrLn stderr $ show t <> " extra timestamps dropped"
    hPutStrLn stderr $ show o <> " lines written"
    hPutStrLn stderr $ percentage o i <> " total lines in/out"
    let perSec = fromIntegral i / toRational dt
    hPutStrLn stderr $ show (round perSec :: Integer) <> " lines/second"

-- | Express n/d as both that ratio and a percentage
percentage :: Int -> Int -> String
percentage n d = let p = int2Double n / int2Double d * 100 :: Double
    in printf "%d/%d (%.2f%%)" n d p

newtype InputLines = InputLines Int

zipExt :: String
zipExt = ".zip.acmi"

readStdin :: ConduitT () BS.ByteString (ResourceT IO) ()
readStdin = sourceHandle stdin

readZip :: FilePath -> IO (ConduitT () BS.ByteString (ResourceT IO) ())
readZip z = do
    unless (zipExt `isSuffixOf` z) $ error "Expected .zip.acmi file"
    withArchive z $ do
        e <- getEntries
        when (M.null e) $ error "empty ZIP archive!"
        let sel = head $ M.keys e
        getEntrySource sel

feed
    :: ConduitT () BS.ByteString (ResourceT IO) ()
    -> IORef Int
    -> Channel Text
    -> IO InputLines
feed src ior c = do
    let go l = do
            evalWriteChannel c l
            atomicModifyIORef' ior $ \p -> (p + 1, ())
    runConduitRes $
        src .| decodeUtf8C .| linesUnboundedC .| mapM_C (liftIO . go)
    -- Hmm: https://gitlab.haskell.org/ghc/ghc/-/issues/22468
    -- https://github.com/haskell/core-libraries-committee/issues/112
    InputLines <$> readIORef ior

newtype DroppedTimeLines = DroppedTimeLines Int
newtype OutputLines = OutputLines Int

-- | Write everything out when we're done.
write
    :: IORef Int
    -> (ConduitT () Text (ResourceT IO) () -> IO ())
    -> Channel Text
    -> IO OutputLines
write iow runner source = do
    let srcC :: ConduitT () Text (ResourceT IO) ()
        srcC = repeatMC (liftIO $ atomically (readChannel source))
            .| mapWhileC id
            .| iterMC (const . liftIO $ atomicModifyIORef' iow $ \p -> (p + 1, ()))
    runner srcC
    OutputLines <$> readIORef iow

writeStdout :: ConduitT () Text (ResourceT IO) () -> IO ()
writeStdout src = runConduitRes $ src .| mapM_C (liftIO . T.putStrLn)

writeZip :: FilePath -> ConduitT () Text (ResourceT IO) () -> IO ()
writeZip z src = do
    let zn = (T.unpack . T.dropEnd (length zipExt) . T.pack $ z) <> "-filtered" <> zipExt
    withBinaryFile zn WriteMode $ \h -> do
        sel <- mkEntrySelector "acmi.txt"
        let eaCompression = M.singleton sel Deflate
            eaEntryComment = M.empty
            eaDeleteComment = M.empty
            eaModTime = M.empty
            eaExtraField = M.empty
            eaDeleteField = M.empty
            eaExtFileAttr = M.empty
            ea = ZI.EditingActions{..}
            pipe = src .| unlinesC .| encodeUtf8C
        (es, des) <- ZI.sinkEntry h sel ZI.GenericOrigin pipe ea
        let cdmap = M.singleton es des
        ZI.writeCD h (Just "Generated with tacview-filter") cdmap

progress :: IORef Int -> IORef Int -> IO ()
progress i o = progress' i o 0

progress' :: IORef Int -> IORef Int -> Int -> IO ()
progress' i o = fix $ \loop n -> do
    i' <- readIORef i
    o' <- readIORef o
    let p = int2Double o' / int2Double (max 1 i') * 100
        cc = clearFromCursorToLineBeginningCode
    T.hPutStr stderr . T.pack $
        printf "%v\r%c %v lines in / %v out (%.0f%%)" cc (spinny n) i' o' p
    threadDelay 100000 -- 20 FPS
    loop (n + 1)

spinny :: Int -> Char
spinny n = case n `mod` 4 of
    0 -> '|'
    1 -> '/'
    2 -> '-'
    3 -> '\\'
    _ -> undefined
