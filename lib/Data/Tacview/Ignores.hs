{-# LANGUAGE StrictData #-}

-- | (Mostly-BMS-specific) filters to ignore objects and events in the Tacview stream
module Data.Tacview.Ignores (filterLines, FilteredLines(..)) where

import Control.Concurrent.Channel
import Control.Concurrent.STM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Set (Set)
import Data.HashMap.Strict qualified as HM
import Data.Maybe
import Data.Tacview
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V

-- The ignore filter.
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
    ifsLinesDropped :: Int
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

-- | We can ignore an event if all of its IDs are in the "ignored" or "events ignored" sets
ignoreableEvent :: Set TacId -> IgnoreFilterState -> Bool
ignoreableEvent es fs = not (HS.null toIgnore) && all (`HS.member` toIgnore) es
    where toIgnore = HS.union fs.ifsIgnored fs.ifsEventsIgnored

unlessMaybe :: Bool -> a -> Maybe a
unlessMaybe b v = if b then Nothing else Just v

newtype FilteredLines = FilteredLines Int

filterLines :: Channel Text -> Channel ParsedLine -> IO FilteredLines
filterLines = filterLines' startState

filterLines'
    :: IgnoreFilterState
    -> Channel Text
    -> Channel ParsedLine
    -> IO FilteredLines
filterLines' !fs source sink = atomically (readChannel source) >>= \case
    Nothing -> pure $ FilteredLines fs.ifsLinesDropped
    Just l -> do
        let p = parseLine l
            (filtered, nextState) = go p
            -- Property lines will update our ignore lists,
            -- then get filtered on the _updated_ version of those, fs'
            go (PropLine pid rawProps) = (p', fs') where
                fs' = updateIgnores pid l fs
                p' = unlessMaybe (HS.member pid fs'.ifsIgnored) (PropLine pid $ sansG rawProps)
            -- Skip a removal line if we're ignoring the object.
            -- The next state is the current one with the ID removed.
            go (RemLine r) = (p', fs') where
                p' = unlessMaybe (HS.member r fs.ifsIgnored) p
                fs' = removeId r fs
            -- Skip an event if it's ignoreable.
            go (EventLine _ is _) = (p', fs) where
                p' = unlessMaybe (ignoreableEvent is fs) p
            -- Pass the rest, no change to state.
            go _ = (Just p, fs)
        mapM_ (atomically . writeChannel sink) filtered
        -- Add one if we filtered a line out.
        let nextCount = fs.ifsLinesDropped + if isNothing filtered then 1 else 0
            nextState' = nextState { ifsLinesDropped = nextCount }
        filterLines' nextState' source sink

-- | The BMS server currently serves BS g-force measurements which are always 0,
-- so then Tacview sessions show everything at 0G.
sansG :: Properties -> Properties
sansG = HM.delete "LateralGForce" . HM.delete "LongitudinalGForce" . HM.delete "VerticalGForce"
