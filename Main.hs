{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Concurrent.Channel
import Control.Concurrent.STM
import Control.Monad
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Maybe
import Data.Ratio
import Data.Tacview
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Vector (Vector)
import Data.Vector qualified as V
import Options.Applicative
import System.IO
import Text.Printf

import GHC.Stack

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

    -- We compose this pipeline back to front, though.
    -- TODO: come up with a more readable way to do this front to back.
    let filterAndWrite c = pipeline (filterLines startState c) (writer 0)
        butFirstDedupTimes c = pipeline (timeDedup 0 0 0.0 c) filterAndWrite
    pipeline reader butFirstDedupTimes

-- Boring I/O stuff start and end our pipeline:

-- | Read stdin and split it into a list of lines
reader :: Channel Text -> IO ()
reader c = do
    lazyLines <- TL.lines <$> TL.getContents
    let strictLines = TL.toStrict <$> lazyLines
    mapM_ (evalWriteChannel c) strictLines

-- | When we're all done, write the lines we ended up with.
writer :: Int -> Channel (Maybe LineIds, Text) -> IO ()
writer !count chan = atomically (readChannel chan) >>= \case
    Nothing -> hPutStrLn stderr $ show count <> " remained"
    Just (_mid, l) -> do
        T.putStrLn l
        writer (count + 1) chan

-- | Pull the (#) off the front of the line and parse the rest as a double.
parseTime :: HasCallStack => Text -> Double
parseTime t = case T.rational (T.tail t) of
    Left e -> error (T.unpack t <> ": " <> e)
    Right (v, _) -> v

-- | Express n/d as both that ratio and a percentage
percentage :: Int -> Int -> String
percentage n d = let p = realToFrac $ n % d * 100 :: Double
    in printf "%d/%d (%.2f%%)" n d p

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


-- Next, the ignore filter.
-- We can either ignore objects entirely, or we can ignore all their events.
-- To do that, we have to keep track of some state:

data IgnoreFilterState = IgnoreFilterState {
    -- | What are objects who should we totally ignore
    --   if their properties contains one of these strings?
    ifsIgnore :: Vector Text,
    -- | What are objects whose events we should ignore
    --   if their properties contains one of these strings?
    ifsUneventful :: Vector Text,
    -- | What objects (by ID) are we currently ignoring?
    ifsIgnored :: HashSet TacId,
    -- | What objects' (by ID) events are we currently ignoring?
    ifsEventsIgnored :: HashSet TacId,
    -- | Track total lines dropped
    ifsLinesDropped :: Int,
    -- | Track total lines processed.
    ifsLinesTotal :: Int
}

-- | The starting /ignore filter/ state.
-- If we want to get fancy, we could fill this out from command line args.
startState :: IgnoreFilterState
startState = IgnoreFilterState{..} where
    -- Tacview actually records all buildings and static objects,
    -- but I never figured out how to give them a size or shape to show up.
    -- I was hoping it would be helpful to at least know when you hit something,
    -- but...
    ifsIgnore = V.fromList [ "Type=Ground+Static" ]
    -- It's really dumb to get hit/timeout messages for ATGM rounds,
    -- which the ground AI shoots like they're machine gun ammo.
    ifsUneventful = V.fromList [
        "Name=BGM-71 TOW",
        "Name=M72 LAW",
        "Name=M47 Dragon ATGM",
        "Name=AT-4" ]
    ifsIgnored = HS.empty
    ifsEventsIgnored = HS.empty
    ifsLinesDropped = 0
    ifsLinesTotal = 0

-- | Given a line and its object ID, update the list of things we're ignoring
--   and the list of things whose events we're ignoring.
updateIgnores :: TacId -> Text -> IgnoreFilterState -> IgnoreFilterState
updateIgnores i l fs = fs { ifsIgnored = newIg, ifsEventsIgnored = newEvIg } where
    (newIg, newEvIg) = go
    go
      -- If the line matches against any of our strings to ignore,
      -- insert the ID into the ignore set
      | any (`T.isInfixOf` l) fs.ifsIgnore = (HS.insert i fs.ifsIgnored, fs.ifsEventsIgnored)
      -- If the line matches against any of our strings of events to ignore,
      -- insert the ID into the events ignore set
      | any (`T.isInfixOf` l) fs.ifsUneventful = (fs.ifsIgnored, HS.insert i fs.ifsEventsIgnored)
      -- Otherwise everything is unchanged.
      | otherwise = (fs.ifsIgnored, fs.ifsEventsIgnored)

-- | Remove the given object ID from the list of things we're ignoring
--   and the list of things whose events we're ignoring.
removeId :: TacId -> IgnoreFilterState -> IgnoreFilterState
removeId i fs = fs { ifsIgnored = newIg, ifsEventsIgnored = newEvIg } where
    newIg = HS.delete i fs.ifsIgnored
    newEvIg = HS.delete i fs.ifsEventsIgnored

-- We can ignore an event if all of its IDs are in the
-- "ignored" or "events ignored" sets
ignoreableEvent :: HashSet TacId -> IgnoreFilterState -> Bool
ignoreableEvent es fs = all (`HS.member` toIgnore) es where
    toIgnore = HS.union fs.ifsIgnored fs.ifsEventsIgnored

unlessMaybe :: Bool -> a -> Maybe a
unlessMaybe b v = if b then Nothing else Just v

-- Our filtered lines are the current line (or nothing if it's filtered out)
-- plus the rest of the list, filtered. Recursion!
filterLines :: IgnoreFilterState -> Channel Text -> Channel (Maybe LineIds, Text) -> IO ()
filterLines !fs source sink = atomically (readChannel source) >>= \case
    Nothing -> hPutStrLn stderr $
        printf "%s lines dropped to ignoreObjects. From the rest,"
            (percentage fs.ifsLinesDropped fs.ifsLinesTotal)
    Just l -> do
        let ids = idsOf l
            (filtered, nextState) = go ids
            -- Property lines will update our ignore lists,
            -- then get filtered on the _updated_ version of those, fs'
            go (Just (PropLine p)) = (l', fs') where
                fs' = updateIgnores p l fs
                l' = unlessMaybe (HS.member p fs'.ifsIgnored) l
            -- Skip a removal line if we're ignoring the object.
            -- The next state is the current one with the ID removed.
            go (Just (RemLine r)) = (l', fs') where
                l' = unlessMaybe (HS.member r fs.ifsIgnored) l
                fs' = removeId r fs
            -- Skip an event if it's ignoreable.
            go (Just (EventLine es)) = (l', fs) where
                l' = unlessMaybe (ignoreableEvent es fs) l
            -- Pass the current line through, no change to state.
            go Nothing = (Just l, fs)
            fi = (ids,) <$> filtered
        mapM_ (evalWriteChannel sink) fi
        -- Add one if we filtered a line out.
        let nextCount = fs.ifsLinesDropped + if isNothing filtered then 1 else 0
            nextTotal = fs.ifsLinesTotal + 1
            nextState' = nextState { ifsLinesDropped = nextCount, ifsLinesTotal = nextTotal }
        filterLines nextState' source sink
