{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Main where

import Control.Concurrent.Channel
import Control.Concurrent.STM
import Control.Monad
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Maybe
import Data.Tacview
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Options.Applicative
import System.IO
import Text.Printf

import GHC.Stack

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

    let dedupTimes sink = pipeline reader (\source -> timeDedup 0 0 0.0 source sink)
        thenFilter sink = pipeline dedupTimes (\source -> filterLines startState source sink)
        thenDeltas sink = pipeline thenFilter (\source -> deltas HM.empty source sink)

    pipeline thenDeltas (writer 0)

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

deltas :: HashMap TacId Properties -> Channel (Maybe LineIds, Text) -> Channel Text -> IO ()
deltas !objects source sink = atomically (readChannel source) >>= \case
    Nothing -> pure ()
    Just (i, l) -> case i of
        Just (PropLine p) -> do
            let props = lineProperties l
                prev = objects HM.!? p
                newProps = case prev of
                    Nothing -> props
                    Just old -> updateProperties old props
                deltaEncoded = case prev of
                    Nothing -> props
                    Just old -> deltaProperties old props
                toWrite = (T.pack . printf "%x," $ p) <> showProperties deltaEncoded
            unless (HM.null deltaEncoded) $ evalWriteChannel sink toWrite
            deltas (HM.insert p newProps objects) source sink
        Just (RemLine p) -> do
            evalWriteChannel sink l
            deltas (HM.delete p objects) source sink
        _other -> do
            evalWriteChannel sink l
            deltas objects source sink

dtHistogram
    :: HashMap TacId Text
    -> HashMap TacId Double
    -> HashMap TacId (IntMap Int)
    -> Double
    -> Channel (Maybe LineIds, Text)
    -> IO ()
dtHistogram !firstMention !lastTimes !histogram !time source =
    atomically (readChannel source) >>= \case
        Nothing -> do
            let printHist (tid, counts) = do
                    T.putStrLn $ fromJust (firstMention HM.!? tid) <> ":"
                    mapM_ printCount (IM.toAscList counts)
                printCount (t, c) = do
                    let t' = (fromIntegral t :: Double) / 100
                    printf "  %f: %d\n" t' c
            mapM_ printHist (HM.toList histogram)

        Just (i, l) -> if T.isPrefixOf "#" l -- If it's a #<time> line
            then -- Parse the new time and move on
                dtHistogram firstMention lastTimes histogram (parseTime l) source
            else case i of
                Just (PropLine p) -> let
                    -- When did we last see this object?
                    prev = fromMaybe 0 $ lastTimes HM.!? p
                    -- How long has it been?
                    dt = time - prev
                    -- Keep track of its first mention (chucking duplicates).
                    newMentions = HM.insertWith (\_new old -> old) p l firstMention
                    -- The last time we saw it was right meow.
                    newLastTimes = HM.insert p time lastTimes
                    -- Add the current dt to our histogram (or start a new one)
                    updatedEntry = addTime dt $ fromMaybe IM.empty (histogram HM.!? p)
                    newHistogram = HM.insert p updatedEntry histogram
                    in dtHistogram newMentions newLastTimes newHistogram time source
                _other -> dtHistogram firstMention lastTimes histogram time source


-- | Pull the (#) off the front of the line and parse the rest as a double.
parseTime :: HasCallStack => Text -> Double
parseTime t = case T.rational (T.tail t) of
    Left e -> error (T.unpack t <> ": " <> e)
    Right (v, _) -> v

-- | Time deduplication filter: Keep track of the last timestamp we saw,
-- and if we see a duplicate one, skip it.
timeDedup :: Int -> Int -> Double -> Channel Text -> Channel Text -> IO ()
timeDedup !count !total !time source sink = atomically (readChannel source) >>= \case
    Nothing -> hPutStrLn stderr $ printf "%s lines were duplicate timestamps. From the rest,"
        (percentage count total)
    Just l -> if T.isPrefixOf "#" l -- If it's a #<time> line
        then do
            let newTime = parseTime l
                duplicateTime = time == newTime
                newCount = if duplicateTime then count + 1 else count
            -- Skip this line if it's a duplicate
            unless duplicateTime $ evalWriteChannel sink l
            timeDedup newCount (total + 1) newTime source sink
        else do -- pass through
            evalWriteChannel sink l
            timeDedup count (total + 1) time source sink


