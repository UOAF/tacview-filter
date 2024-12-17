{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Main where

import Codec.Archive.Zip
import Control.Concurrent.Channel
import Control.Concurrent.STM
import Control.Monad
import Conduit (liftIO)
import Data.Conduit ((.|))
import Data.Conduit qualified as C
import Data.Conduit.Combinators qualified as C
import Data.IORef
import Data.List
import Data.Map.Strict qualified as M
import Data.Ratio
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time.Clock.POSIX
import Options.Applicative
import System.IO
import Text.Printf

import Delta
import Ignores

newtype Args = Args (Maybe FilePath)

parseArgs :: Parser Args
parseArgs = fmap Args $ optional . strArgument $ mconcat [
    help "Zipped ACMI to filter. Otherwise reads from stdin and writes to stdout",
    metavar "recording.zip.acmi"
    ]

-- --help text
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
runFilter (Args maybeZip) = do
    start <- getPOSIXTime

    -- Hoare Was Right.
    -- The program is pipelined (see Data.Concurrency.Channel):
    -- Major steps are run in their own task.
    -- They filters or modify lines, then pass the remainders to the next stage of the pipe.
    -- This parallelizes trivially; the runtime runs each task in a free thread.
    let filterInput = maybe (reader 0) readZip maybeZip
        filterOutput = maybe (write 0) writeZip maybeZip
        ignore sink = pipeline filterInput (\source -> filterLines Ignores.startState source sink)
        thenDeltas sink = pipeline ignore (\source -> deltas Delta.startState source sink)
        thenDropTimes = pipeline thenDeltas filterOutput

    -- Gather up all our stats, placed in newtypes for easier readability here.
    (((InputLines i, FilteredLines f), DecimatedLines d), OutputLines o) <- thenDropTimes

    end <- getPOSIXTime
    let dt = end - start
        dts = printf "%.2f" (realToFrac dt :: Double)
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
percentage n d = let p = realToFrac $ n % d * 100 :: Double
    in printf "%d/%d (%.2f%%)" n d p

newtype InputLines = InputLines Int

-- | Read stdin and send it line-by-line into our pipeline.
reader :: Int -> Channel Text -> IO InputLines
reader !l c = do
    eof <- isEOF
    if eof
        then pure $ InputLines l
        else do
            T.getLine >>= evalWriteChannel c
            reader (l + 1) c

zipExt :: String
zipExt = ".zip.acmi"

readZip :: FilePath -> Channel Text -> IO InputLines
readZip z c = do
    unless (zipExt `isSuffixOf` z) $ error "Expected .zip.acmi file"
    withArchive z $ do
        e <- getEntries
        when (M.null e) $ error "empty ZIP archive!"
        let sel = head $ M.keys e
        l <- sourceEntry sel $
            C.decodeUtf8 .| C.linesUnbounded .| C.iterM (liftIO . evalWriteChannel c) .| C.length
        pure $ InputLines l

newtype DroppedTimeLines = DroppedTimeLines Int
newtype OutputLines = OutputLines Int

-- | Write everything out when we're done.
write :: Int -> Channel Text -> IO OutputLines
write !count source = atomically (readChannel source) >>= \case
    Nothing -> pure $ OutputLines count
    Just l -> do
        T.putStrLn l
        write (count + 1) source

writeZip :: FilePath -> Channel Text -> IO OutputLines
writeZip z c = do
    let zn = (T.unpack . T.dropEnd (length zipExt) . T.pack $ z) <> "-filtered" <> zipExt
    ol <- newIORef 0
    createArchive zn $ do
        sel <- mkEntrySelector "acmi.txt"
        let src !i = liftIO (atomically (readChannel c)) >>= \case
                Nothing -> do
                    liftIO $ writeIORef ol i -- HACK HACK - make sinkEntry return a value?
                    pure ()
                Just l -> do
                    C.yield l
                    src (i + 1)
            pipe = src 0 .| C.unlines .| C.encodeUtf8
        sinkEntry Deflate pipe sel
    OutputLines <$> readIORef ol


