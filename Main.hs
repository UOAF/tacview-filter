{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Main where

import Control.Concurrent.Channel
import Control.Concurrent.STM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Tacview
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Vector (Vector)
import Data.Vector qualified as V
import Options.Applicative
import System.IO

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
    pipeline reader (\c -> pipeline (filterLines startState c) writer)
    -- let filtered = filteredLines startState ls -- Filter it...
    -- mapM_ T.putStrLn filtered -- And print it back out, line at a time.

-- Boring I/O stuff: read stdin and split it into a list of lines.
reader :: Channel Text -> IO ()
reader c = do
    lazyLines <- TL.lines <$> TL.getContents
    let strictLines = TL.toStrict <$> lazyLines
    mapM_ (atomically . writeChannel c) strictLines

writer :: Channel Text -> IO ()
writer c =  do
    mv <- atomically $ readChannel c
    case mv of
        Nothing -> pure ()
        Just v -> do
            T.putStrLn v
            writer c

-- | Things we track as we iterate through the lines:
data FilterState = FilterState {
    -- | What are objects who should we totally ignore
    --   if their properties contains one of these strings?
    fsIgnore :: Vector Text,
    -- | What are objects whose events we should ignore
    --   if their properties contains one of these strings?
    fsUneventful :: Vector Text,
    -- | What objects (by ID) are we currently ignoring?
    fsIgnored :: HashSet TacId,
    -- | What objects' (by ID) events are we currently ignoring?
    fsEventsIgnored :: HashSet TacId
}

startState :: FilterState
startState = FilterState{..} where
    -- Tacview actually records all buildings and static objects,
    -- but I never figured out how to give them a size or shape to show up.
    -- I was hoping it would be helpful to at least know when you hit something,
    -- but...
    fsIgnore = V.fromList [ "Type=Ground+Static" ]
    -- It's really dumb to get hit/timeout messages for ATGM rounds,
    -- which the ground AI shoots like they're machine gun ammo.
    fsUneventful = V.fromList [
        "Name=BGM-71 TOW",
        "Name=M72 LAW",
        "Name=M47 Dragon ATGM",
        "Name=AT-4" ]
    fsIgnored = HS.empty
    fsEventsIgnored = HS.empty

-- | Given a line and its object ID, update the list of things we're ignoring
--   and the list of things whose events we're ignoring.
updateIgnores :: TacId -> Text -> FilterState -> FilterState
updateIgnores i l fs = fs { fsIgnored = newIg, fsEventsIgnored = newEvIg } where
    (newIg, newEvIg) = go
    go
      -- If the line matches against any of our strings to ignore,
      -- insert the ID into the ignore set
      | any (`T.isInfixOf` l) fs.fsIgnore = (HS.insert i fs.fsIgnored, fs.fsEventsIgnored)
      -- If the line matches against any of our strings of events to ignore,
      -- insert the ID into the events ignore set
      | any (`T.isInfixOf` l) fs.fsUneventful = (fs.fsIgnored, HS.insert i fs.fsEventsIgnored)
      -- Otherwise everything is unchanged.
      | otherwise = (fs.fsIgnored, fs.fsEventsIgnored)

-- | Remove the given object ID from the list of things we're ignoring
--   and the list of things whose events we're ignoring.
removeId :: TacId -> FilterState -> FilterState
removeId i fs = fs { fsIgnored = newIg, fsEventsIgnored = newEvIg } where
    newIg = HS.delete i fs.fsIgnored
    newEvIg = HS.delete i fs.fsEventsIgnored

-- We can ignore an event if all of its IDs are in the
-- "ignored" or "events ignored" sets
ignoreableEvent :: HashSet TacId -> FilterState -> Bool
ignoreableEvent es fs = all (`HS.member` toIgnore) es where
    toIgnore = HS.union fs.fsIgnored fs.fsEventsIgnored

-- Our filtered lines are the current line (or nothing if it's filtered out)
-- plus the rest of the list, filtered. Recursion!
filterLines :: FilterState -> Channel Text -> Channel Text -> IO ()
filterLines fs source sink = do
    mv <- atomically $ readChannel source
    case mv of
        Nothing -> pure ()
        Just l -> do
            let (filtered, nextState) = go $ idsOf l
                -- Property lines will update our ignore lists,
                -- then get filtered on the _updated_ version of those, fs'
                go (Just (PropLine p)) = (l', fs') where
                    fs' = updateIgnores p l fs
                    l' = [l | not (HS.member p fs'.fsIgnored)]
                -- Skip a removal line if we're ignoring the object.
                -- The next state is the current one with the ID removed.
                go (Just (RemLine r)) = (l', fs') where
                    l' = [l | not (HS.member r fs.fsIgnored)]
                    fs' = removeId r fs
                -- Skip an event if it's ignoreable.
                go (Just (EventLine es)) = (l', fs) where
                    l' = [l | not (ignoreableEvent es fs)]
                -- Pass the current line through, no change to state.
                go Nothing = ([l], fs)
            mapM_ (atomically . writeChannel sink) filtered
            filterLines nextState source sink
