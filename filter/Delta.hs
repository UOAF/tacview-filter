{-# LANGUAGE StrictData #-}

module Delta (deltas, DeltaFilterState, startState) where

import Control.Concurrent.Channel
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Maybe
import Data.Tacview
import Data.Tacview.Delta
import Data.Text (Text)
import Data.Text qualified as T
import Text.Printf

-- We might not have written in in a bit,
-- so make sure its last known state goes out.
closeOut :: Channel Text -> (TacId, ObjectState) -> IO ()
closeOut sink (i, o) = mapM_ (evalWriteChannel sink) $
    deltaEncode i o.osLastWritten o.osCurrent

data DeltaFilterState = DeltaFilterState {
    dfsObjects :: HashMap TacId ObjectState,
    dfsNow :: Double,
    dfsLastWrittenTime :: Double
}

startState :: DeltaFilterState
startState = DeltaFilterState HM.empty 0.0 0.0

deltas
    :: DeltaFilterState
    -> Channel (ParsedLine, Text)
    -> Channel Text -> IO ()
deltas !s source sink = atomically (readChannel source) >>= \case
    -- Delta-encode all remaining objects on the way out
    -- so we don't drop any last-second changes.
    Nothing -> mapM_ (closeOut sink) $ HM.toList s.dfsObjects
    Just (p, l) -> deltas' s p l source sink

deltas'
    :: DeltaFilterState
    -> ParsedLine
    -> Text
    -> Channel (ParsedLine, Text)
    -> Channel Text
    -> IO ()
deltas' !s p l source sink = let
    -- Helper to write a timestamp when we need a new one.
    writeTimestamp = when (s.dfsNow /= s.dfsLastWrittenTime) $
        evalWriteChannel sink $ "#" <> (shaveZeroes . T.pack $ printf "%0.2f" s.dfsNow)
    -- Helper to remove the object from the set we're tracking, taking the ID
    axeIt x = do
        -- We might not have written in in a bit,
        -- so make sure its last known state goes out.
        case s.dfsObjects HM.!? x of
            Just going -> closeOut sink (x, going)
            Nothing -> pure ()
        writeTimestamp
        evalWriteChannel sink l
        let newState = s {
                dfsLastWrittenTime = s.dfsNow,
                dfsObjects = HM.delete x s.dfsObjects
            }
        deltas newState source sink
    -- Helper to pass the line through without doing anything, taking the time.
    passthrough = do
        writeTimestamp
        evalWriteChannel sink l
        deltas s { dfsLastWrittenTime = s.dfsNow } source sink
    in case p of
        PropLine pid props -> do
            -- Is it anything we're tracking?
            let prev = s.dfsObjects HM.!? pid
                -- Update the properties we're tracking.
                (newState, deltaLine) = updateObject prev s.dfsNow pid props
            -- If we have a delta line...
            maybeNewTime <- forM deltaLine $ \d -> do
                writeTimestamp
                evalWriteChannel sink d
                pure s.dfsNow
            let nextState = s {
                    dfsObjects = HM.insert pid newState s.dfsObjects,
                    dfsLastWrittenTime = fromMaybe s.dfsLastWrittenTime maybeNewTime
                }
            deltas nextState source sink
        RemLine pid -> axeIt pid
        -- If it's a "left the area" event, assume its deletion will
        -- come next. We want to write out any last properties before going.
        EventLine es -> if T.isInfixOf "Event=LeftArea" l
            then assert (HS.size es == 1) $ do
                axeIt (head $ HS.toList es)
            else passthrough
        -- If it's a #<time> line, note the new time but dont write.
        TimeLine t -> deltas s { dfsNow = t } source sink
        _ -> passthrough
