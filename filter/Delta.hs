{-# LANGUAGE StrictData #-}

module Delta (deltas, DeltaFilterState, startState) where

import Control.Concurrent.Channel
import Control.Concurrent.STM
import Control.Exception
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Maybe
import Data.Tacview
import Data.Tacview.Delta
import Data.Text qualified as T

data DeltaFilterState = DeltaFilterState {
    liveObjects :: HashMap TacId ObjectState,
    now :: Double,
    lastWrittenTime :: Double
}

startState :: DeltaFilterState
startState = DeltaFilterState HM.empty 0.0 0.0

writeOut :: Channel ParsedLine -> [ParsedLine] -> IO ()
writeOut c ts = mapM_ (evalWriteChannel c) ts

deltas
    :: DeltaFilterState
    -> Channel ParsedLine
    -> Channel ParsedLine -> IO ()
deltas !dfs source sink = atomically (readChannel source) >>= \case
    -- Delta-encode all remaining objects on the way out
    -- so we don't drop any last-second changes.
    Nothing -> do
        let closeLine :: TacId -> ObjectState -> Maybe ParsedLine
            closeLine i s = PropLine i <$> closeOut s
            allClosed = catMaybes $ uncurry closeLine <$> HM.toList dfs.liveObjects
        writeOut sink allClosed
    Just p -> do
        let (newLines, newState) = deltas' dfs p
        writeOut sink $ catMaybes newLines
        deltas newState source sink

deltas'
    :: DeltaFilterState
    -> ParsedLine
    -> ([Maybe ParsedLine], DeltaFilterState)
deltas' !dfs p = let
    -- Helper to write a timestamp when we need a new one.
    writeTimestamp :: Maybe ParsedLine
    writeTimestamp = if (dfs.now /= dfs.lastWrittenTime)
        then Just $ TimeLine dfs.now
        else Nothing
    -- Helper to remove the object from the set we're tracking, taking the ID
    axeIt :: TacId -> ([Maybe ParsedLine], DeltaFilterState)
    axeIt x = let
        -- We might not have written in in a bit,
        -- so make sure its last known state goes out.
        co :: Maybe Properties
        co = dfs.liveObjects HM.!? x >>= closeOut
        newState = dfs {
            lastWrittenTime = dfs.now,
            liveObjects = HM.delete x dfs.liveObjects
        }
        in ([PropLine x <$> co, writeTimestamp, Just p], newState)
    -- Helper to pass the line through without doing anything, taking the time.
    passthrough :: ([Maybe ParsedLine], DeltaFilterState)
    passthrough = let
        newState = dfs { lastWrittenTime = dfs.now }
        in ([writeTimestamp, Just p], newState)
    in case p of
        PropLine pid props -> let
            -- Is it anything we're tracking?
            prev = dfs.liveObjects HM.!? pid
            -- Update the properties we're tracking.
            (newObjState, deltaLine) = updateObject prev dfs.now props
            newObjs = HM.insert pid newObjState dfs.liveObjects
            newState = dfs { liveObjects = newObjs }
            -- If we have a delta line, write some stuff...
            in case deltaLine of
                Just dl -> (
                    [writeTimestamp, Just $ PropLine pid dl],
                    newState { lastWrittenTime = dfs.now }
                    )
                Nothing -> ([], newState) -- Otherwise just update liveObjects
        RemLine pid -> axeIt pid
        -- If it's a "left the area" event, assume its deletion will
        -- come next. We want to write out any last properties before going.
        EventLine es l -> if T.isInfixOf "Event=LeftArea" l
            then assert (HS.size es == 1) $ do
                axeIt (head $ HS.toList es)
            else passthrough
        -- If it's a #<time> line, note the new time but dont write.
        TimeLine t -> ([], dfs { now = t })
        -- We don't do anything with global config lines.
        GlobalLine _ -> passthrough
