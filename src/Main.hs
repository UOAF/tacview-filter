{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Main where

import Control.Concurrent.Channel
import Control.Concurrent.STM
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.HashMap.Strict qualified as HM
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Options.Applicative
import System.IO
import Text.Printf

import Delta
import Ignores
import Utils

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
    -- Hoare Was Right.
    -- The program is pipelined (see Data.Concurrency.Channel):
    -- each step is run in its own task, filters lines out,
    -- and passes the remainders to the next stage of the pipe.
    -- This parallelizes trivially; the runtime runs each task in a free thread.

    let ignore sink = pipeline reader (\source -> filterLines startState source sink)
        thenDeltas sink = pipeline ignore (\source -> deltas 0.0 HM.empty source sink)
        thenDropTimes sink = pipeline thenDeltas (\source -> timeDrop 0 0 Nothing source sink)

    pipeline thenDropTimes (writer 0)

-- Boring I/O stuff start and end our pipeline:

-- | Read stdin and split it into a list of lines
reader :: Channel Text -> IO ()
reader c = do
    lazyLines <- TL.lines <$> TL.getContents
    let strictLines = TL.toStrict <$> lazyLines
    mapM_ (evalWriteChannel c) strictLines

-- | When we're all done, write the lines we ended up with.
writer :: Int -> Channel Text -> IO ()
writer !count chan = atomically (readChannel chan) >>= \case
    Nothing -> hPutStrLn stderr $ show count <> " remained"
    Just l -> do
        T.putStrLn l
        writer (count + 1) chan

-- | Add a given time to the given histogram
addTime :: Double -> IntMap Int -> IntMap Int
addTime t = IM.insertWith (+) timeKey 1 where
    timeKey = round $ t * 100

-- | Drop adjacent timestamps
timeDrop :: Int -> Int -> Maybe Text -> Channel Text -> Channel Text -> IO ()
timeDrop !count !total lastTime source sink = atomically (readChannel source) >>= \case
    Nothing -> hPutStrLn stderr $ printf "%s lines were duplicate timestamps. From the rest,"
        (percentage count total)
    Just l -> if T.isPrefixOf "#" l -- If it's a #<time> line
        then timeDrop (count + 1) (total + 1) (Just l) source sink
        else do -- If it's not a time line, drop the one we've been holding.
            mapM_ (evalWriteChannel sink) lastTime
            evalWriteChannel sink l
            let timesWritten = if isJust lastTime then 1 else 0
            timeDrop (count - timesWritten) (total + 1) Nothing source sink


