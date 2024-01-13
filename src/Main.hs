{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Main where

import Control.Concurrent.Channel
import Control.Concurrent.STM
import Data.Time.Clock.POSIX
import Data.Maybe
import Data.Ratio
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options.Applicative
import System.IO
import Text.Printf

import Delta
import Ignores

-- No command line args at the moment...
-- Add flags to --ignore additional text?
-- Or we can just change this short program...
data Args = Args

parseArgs :: Parser Args
parseArgs = pure Args

-- --help text
main :: IO ()
main = do
    -- lol Windows
    hSetNewlineMode stdin noNewlineTranslation
    hSetNewlineMode stdout noNewlineTranslation
    hSetNewlineMode stderr noNewlineTranslation

    let parser = customExecParser (prefs showHelpOnError) parseInfo
        parseInfo = info (parseArgs <**> helper) $
            progDesc "Reads ACMI on stdin, filters crap, writes to stdout"
    parser >>= runFilter

runFilter :: Args -> IO ()
runFilter _ = do
    start <- getPOSIXTime

    -- Hoare Was Right.
    -- The program is pipelined (see Data.Concurrency.Channel):
    -- Major steps are run in their own task.
    -- They filters or modify lines, then pass the remainders to the next stage of the pipe.
    -- This parallelizes trivially; the runtime runs each task in a free thread.
    let ignore sink = pipeline (reader 0) (\source -> filterLines Ignores.startState source sink)
        thenDeltas sink = pipeline ignore (\source -> deltas Delta.startState source sink)
        thenDropTimes = pipeline thenDeltas (timeDropAndWrite 0 0 Nothing)

    -- Gather up all our stats, placed in newtypes for easier readability here.
    (((InputLines i, FilteredLines f), DecimatedLines d),
      (DroppedTimeLines t, OutputLines o)) <- thenDropTimes

    end <- getPOSIXTime
    let dt = end - start
        dts = printf "%.2f" (realToFrac dt :: Double)
    hPutStrLn stderr $ "in " <> dts <> " seconds"
    hPutStrLn stderr $ show i <> " lines read"
    hPutStrLn stderr $ show f <> " lines ignored"
    hPutStrLn stderr $ show d <> " lines decimated"
    hPutStrLn stderr $ show t <> " extra timestamps dropped"
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

newtype DroppedTimeLines = DroppedTimeLines Int
newtype OutputLines = OutputLines Int

-- | Drop adjacent timestamps, then write the lines we end up with.
timeDropAndWrite :: Int -> Int -> Maybe Text -> Channel Text -> IO (DroppedTimeLines, OutputLines)
timeDropAndWrite !count !total lastTime source = atomically (readChannel source) >>= \case
    Nothing -> pure (DroppedTimeLines count, OutputLines total)
    Just l -> if T.isPrefixOf "#" l -- If it's a #<time> line
        then timeDropAndWrite (count + 1) total (Just l) source
        else do -- If it's not a time line, drop the one we've been holding.
            mapM_ T.putStrLn lastTime
            T.putStrLn l
            let timesWritten = if isJust lastTime then 1 else 0
                newCount = count - timesWritten
                newTotal = total + 1 + timesWritten
            timeDropAndWrite newCount newTotal Nothing source
