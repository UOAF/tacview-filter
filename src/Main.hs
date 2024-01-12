{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Main where

import Control.Concurrent.Channel
import Control.Concurrent.STM
import Control.Monad
import Data.HashMap.Strict qualified as HM
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
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
        thenDropTimes = pipeline thenDeltas (timeDropAndWrite 0 0 Nothing)

    void thenDropTimes

-- Boring I/O stuff start and end our pipeline:

-- | Read stdin and split it into a list of lines
reader :: Channel Text -> IO ()
reader c = do
    eof <- isEOF
    unless eof $ do
        T.getLine >>= evalWriteChannel c
        reader c

writer :: Int -> Channel Text -> IO ()
writer !count chan = atomically (readChannel chan) >>= \case
    Nothing -> hPutStrLn stderr $ show count <> " remained"
    Just l -> do
        T.putStrLn l
        writer (count + 1) chan

-- | Drop adjacent timestamps, then write the lines we end up with.
timeDropAndWrite :: Int -> Int -> Maybe Text -> Channel Text -> IO ()
timeDropAndWrite !count !total lastTime source = atomically (readChannel source) >>= \case
    Nothing -> do
        hPutStrLn stderr $ printf "%s lines were extra timestamps." (percentage count total)
    Just l -> if T.isPrefixOf "#" l -- If it's a #<time> line
        then timeDropAndWrite (count + 1) (total + 1) (Just l) source
        else do -- If it's not a time line, drop the one we've been holding.
            mapM_ T.putStrLn lastTime
            T.putStrLn l
            let timesWritten = if isJust lastTime then 1 else 0
            timeDropAndWrite (count - timesWritten) (total + 1) Nothing source


