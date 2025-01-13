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
import Data.Text (Text)
import Data.Text qualified as T
import Text.Printf

data DeltaFilterState = DeltaFilterState {
    liveObjects :: HashMap TacId ObjectState,
    now :: Double,
    lastWrittenTime :: Double
}

startState :: DeltaFilterState
startState = DeltaFilterState HM.empty 0.0 0.0

writeOut :: Channel Text -> [Text] -> IO ()
writeOut c ts = mapM_ (evalWriteChannel c) ts

deltas
    :: DeltaFilterState
    -> Channel (ParsedLine, Text)
    -> Channel Text -> IO ()
deltas !dfs source sink = atomically (readChannel source) >>= \case
    -- Delta-encode all remaining objects on the way out
    -- so we don't drop any last-second changes.
    Nothing -> do
        let allClosed = catMaybes $ uncurry closeOut <$> HM.toList dfs.liveObjects
        writeOut sink allClosed
    Just (p, l) -> do
        let (newLines, newState) = deltas' dfs p l
        writeOut sink $ catMaybes newLines
        deltas newState source sink

deltas'
    :: DeltaFilterState
    -> ParsedLine
    -> Text
    -> ([Maybe Text], DeltaFilterState)
deltas' !dfs p l = let
    -- Helper to write a timestamp when we need a new one.
    writeTimestamp :: Maybe Text
    writeTimestamp = if (dfs.now /= dfs.lastWrittenTime)
        then Just $ "#" <> (shaveZeroes . T.pack $ printf "%f" dfs.now)
        else Nothing
    -- Helper to remove the object from the set we're tracking, taking the ID
    axeIt :: TacId -> ([Maybe Text], DeltaFilterState)
    axeIt x = let
        -- We might not have written in in a bit,
        -- so make sure its last known state goes out.
        co :: Maybe Text
        co = dfs.liveObjects HM.!? x >>= closeOut x
        newState = dfs {
            lastWrittenTime = dfs.now,
            liveObjects = HM.delete x dfs.liveObjects
        }
        in ([co, writeTimestamp, Just l], newState)
    -- Helper to pass the line through without doing anything, taking the time.
    passthrough :: ([Maybe Text], DeltaFilterState)
    passthrough = let
        newState = dfs { lastWrittenTime = dfs.now }
        in ([writeTimestamp, Just l], newState)
    in case p of
        PropLine pid props -> let
            -- Is it anything we're tracking?
            prev = dfs.liveObjects HM.!? pid
            -- Update the properties we're tracking.
            (newObjState, deltaLine) = updateObject prev dfs.now pid props
            newObjs = HM.insert pid newObjState dfs.liveObjects
            newState = dfs { liveObjects = newObjs }
            -- If we have a delta line, write some stuff...
            in case deltaLine of
                Just dl -> ([writeTimestamp, Just dl], newState { lastWrittenTime = dfs.now })
                Nothing -> ([], newState) -- Otherwise just update liveObjects
        RemLine pid -> axeIt pid
        -- If it's a "left the area" event, assume its deletion will
        -- come next. We want to write out any last properties before going.
        EventLine es -> if T.isInfixOf "Event=LeftArea" l
            then assert (HS.size es == 1) $ do
                axeIt (head $ HS.toList es)
            else passthrough
        -- If it's a #<time> line, note the new time but dont write.
        TimeLine t -> ([], dfs { now = t })
        -- We don't do anything with global config lines.
        GlobalLine -> passthrough
