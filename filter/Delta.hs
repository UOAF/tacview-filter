{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Delta (deltas, DecimatedLines(..), DeltaFilterState, startState) where

import Control.Concurrent.Channel
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Maybe
import Data.Tacview
import Data.Text (Text)
import Data.Text qualified as T
import Text.Printf

-- We have two jobs here which make up the bulk of our space savings:
-- 1. Delta-encode property lines: don't repeat properties which were already mentioned
-- 2. Decimate property lines: Tacview only needs a few Hertz to put on a good show.
--    (If you want to do numerical analysis flying things, Tacview data is already
--    severely quantized you're better off dumping raw data from the game by other means.)

data ObjectState = ObjectState {
    osCurrent :: Properties,
    osLastWritten :: Properties,
    osNextWrite :: Double,
    osRate :: Double
}

-- | Given a new property line for the object,
-- compute the next state and a delta-encoded line to print.
-- The ID is provided only for printing purposes
updateObject :: Maybe ObjectState -> Double -> TacId -> Properties -> (ObjectState, Maybe Text)
updateObject maybePrevious now i props = case maybePrevious of
    Nothing -> let
        -- There was no previous record of this object.
        -- Everything becomes the property set we're given,
        -- and we should write a line immediately.
        osCurrent = props
        osLastWritten = props
        osRate = rateOf $
            -- showProperty is a no-op for non-position/numeric properties,
            -- which "Type" had better be.
            showProperty <$> props HM.!? "Type"
        osNextWrite = now + osRate
        in (ObjectState{..}, Just $ showLine i props)
    Just prev -> let
        -- We have a previous record of this object.
        -- Merge its properties into the current set,
        merged = updateProperties prev.osCurrent props
        -- and decide if it's been long enough we should write a new line.
        in if now >= prev.osNextWrite
            then let
                -- NB: next.current == next.lastWritten (since we're writing now!)
                next = prev {
                    osCurrent = merged,
                    osLastWritten = merged,
                    osNextWrite = now + prev.osRate
                }
                -- ...but be sure to delta lastWritten as it just was
                in (next, deltaEncode i prev.osLastWritten merged)
            else (prev { osCurrent = merged }, Nothing)

-- | Delta-encode an object, generating a line with only properties that changed.
-- Returns Just the line, or Nothing if there's no changes.
deltaEncode :: TacId -> Properties -> Properties -> Maybe Text
deltaEncode i old new = let
    deltaProps = deltaProperties old new
    in if HM.null deltaProps
        -- Don't write if the delta-encoded version is empty (nothing changed).
        then Nothing
        else Just $ showLine i deltaProps

showLine :: TacId -> Properties -> Text
showLine i props = (T.pack . printf "%x," $ i) <> showProperties props

-- | Update rates of various object types, or 1 Hz by default.
-- Uses recommendations from https://www.tacview.net/documentation/realtime/en/
rateOf :: Maybe Text -> Double
rateOf (Just l)
    | T.isInfixOf "FixedWing" l = 1 / 10
    | T.isInfixOf "Missile" l = 1 / 8
    | T.isInfixOf "Air" l = 1 / 5
    | T.isInfixOf "Projectile" l = 1 / 2
    | T.isInfixOf "Bomb" l = 1 / 2
    | T.isInfixOf "Decoy" l = 1 / 2
    | T.isInfixOf "Shrapnel" l = 1 / 2
    | T.isInfixOf "Ground" l = 1 / 2
    | otherwise = 1.0
rateOf Nothing = 1.0

-- We might not have written in in a bit,
-- so make sure its last known state goes out.
closeOut :: Channel Text -> (TacId, ObjectState) -> IO ()
closeOut sink (i, o) = mapM_ (evalWriteChannel sink) $
    deltaEncode i o.osLastWritten o.osCurrent

data DeltaFilterState = DeltaFilterState {
    dfsObjects :: HashMap TacId ObjectState,
    dfsNow :: Double,
    dfsLastWrittenTime :: Double,
    dfsLinesDropped :: Int
}

startState :: DeltaFilterState
startState = DeltaFilterState HM.empty 0.0 0.0 0

newtype DecimatedLines = DecimatedLines Int

deltas
    :: DeltaFilterState
    -> Channel (Maybe LineIds, Text)
    -> Channel Text -> IO DecimatedLines
deltas !s source sink = atomically (readChannel source) >>= \case
    -- Delta-encode all remaining objects on the way out
    -- so we don't drop any last-second changes.
    Nothing -> do
        mapM_ (closeOut sink) $ HM.toList s.dfsObjects
        pure $ DecimatedLines s.dfsLinesDropped
    Just (mid, l) -> deltas' mid l s source sink

deltas'
    :: Maybe LineIds
    -> Text
    -> DeltaFilterState
    -> Channel (Maybe LineIds, Text)
    -> Channel Text
    -> IO DecimatedLines
deltas' mid l !s source sink = let
    -- Helper to write a timestamp when we need a new one.
    writeTimestamp = do
        when (s.dfsNow /= s.dfsLastWrittenTime) $
            evalWriteChannel sink $ "#" <> (shaveZeroes . T.pack $ printf "%f" s.dfsNow)
        pure s.dfsNow
    -- Helper to remove the object from the set we're tracking, taking the ID
    axeIt x = do
        -- We might not have written in in a bit,
        -- so make sure its last known state goes out.
        case s.dfsObjects HM.!? x of
            Just going -> closeOut sink (x, going)
            Nothing -> pure ()
        newLastWritten <- writeTimestamp
        evalWriteChannel sink l
        let newState = s {
                dfsLastWrittenTime = newLastWritten,
                dfsObjects = HM.delete x s.dfsObjects
            }
        deltas newState source sink
    -- Helper to pass the line through without doing anything, taking the time.
    passthrough = do
        newLastWritten <- writeTimestamp
        evalWriteChannel sink l
        deltas s { dfsLastWrittenTime = newLastWritten } source sink
    in case mid of
        Just (PropLine p) -> do
            -- Parse the line's properties and see if it's anything we're tracking.
            let props = sansG . lineProperties $ l
                prev = s.dfsObjects HM.!? p
                -- Update the properties we're tracking.
                (newState, deltaLine) = updateObject prev s.dfsNow p props
            -- If we have a delta line...
            maybeNewLastWrittenTime <- forM deltaLine $ \d -> do
                nlw <- writeTimestamp
                evalWriteChannel sink d
                pure nlw
            let nextState = s {
                    dfsObjects = HM.insert p newState s.dfsObjects,
                    dfsLastWrittenTime = fromMaybe s.dfsLastWrittenTime maybeNewLastWrittenTime,
                    dfsLinesDropped = s.dfsLinesDropped + if isNothing deltaLine then 1 else 0
                }
            deltas nextState source sink
        Just (RemLine p) -> axeIt p
        -- If it's a "left the area" event, assume its deletion will
        -- come next. We want to write out any last properties before going.
        Just (EventLine es) -> if T.isInfixOf "Event=LeftArea" l
            then assert (HS.size es == 1) $ do
                axeIt (head $ HS.toList es)
            else passthrough
        Nothing -> if T.isPrefixOf "#" l
            -- If it's a #<time> line, note the new time but dont write.
            then deltas s { dfsNow = parseTime l } source sink
            else passthrough

-- | The BMS server currently serves BS g-force measurements which are always 0,
-- so then Tacview sessions show everything at 0G.
sansG :: Properties -> Properties
sansG = HM.delete "LateralGForce" . HM.delete "LongitudinalGForce" . HM.delete "VerticalGForce"
