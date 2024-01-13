{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Delta (deltas, DecimatedLines(..), DeltaFilterState, startState) where

import Control.Concurrent.Channel
import Control.Concurrent.STM
import Control.Exception
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Maybe
import Data.Tacview
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Text.Printf

import GHC.Stack

data ObjectState = ObjectState {
    osCurrent :: Properties,
    osLastWritten :: Properties,
    osNextWrite :: Double,
    osRate :: Double
}

updateObject :: Maybe ObjectState -> Double -> TacId -> Properties -> (ObjectState, Maybe Text)
updateObject maybePrevious now i props = case maybePrevious of
    -- There was no previous record of this object.
    -- Everything becomes the property set we're given,
    -- and we should write a line immediately.
    Nothing -> let
        osCurrent = props
        osLastWritten = props
        osRate = rateOf $
            -- showProperty is a no-op for non-position properties,
            -- which this had better be.
            showProperty <$> props HM.!? "Type"
        osNextWrite = now + osRate
        in (ObjectState{..}, Just $ showLine i props)
    -- We have a previous record of this object.
    Just prev -> let
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
    dfsLinesDropped :: Int
}

startState :: DeltaFilterState
startState = DeltaFilterState HM.empty 0.0 0

-- | Pull the (#) off the front of the line and parse the rest as a double.
parseTime :: HasCallStack => Text -> Double
parseTime t = case T.rational (T.tail t) of
    Left e -> error (T.unpack t <> ": " <> e)
    Right (v, _) -> v

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
    -- Remove the object from the set we're tracking.
    axeIt x = do
        -- We might not have written in in a bit,
        -- so make sure its last known state goes out.
        case s.dfsObjects HM.!? x of
            Just going -> closeOut sink (x, going)
            Nothing -> pure ()
        evalWriteChannel sink l
        deltas s { dfsObjects = HM.delete x s.dfsObjects } source sink
    -- Pass the line through without doing anything.
    passthrough t = do
        evalWriteChannel sink l
        deltas s { dfsNow = t } source sink
    in case mid of
        Just (PropLine p) -> do
            -- Parse the line's properties and see if it's anything we're tracking.
            let props = lineProperties l
                prev = s.dfsObjects HM.!? p
                -- Update the properties we're tracking.
                (newState, deltaLine) = updateObject prev s.dfsNow p props
            mapM_ (evalWriteChannel sink) deltaLine
            let nextState = s {
                    dfsObjects = HM.insert p newState s.dfsObjects,
                    dfsLinesDropped = s.dfsLinesDropped + if isNothing deltaLine then 1 else 0
                }
            deltas nextState source sink
        Just (RemLine p) -> axeIt p
        -- If it's a "left the area" event, assume its deletion will
        -- come next. We want to write out any last properties before going.
        Just (EventLine es) -> if T.isInfixOf "Event=LeftArea" l
            then assert (HS.size es == 1) $ do
                axeIt (head $ HS.toList es)
            else passthrough s.dfsNow
        Nothing -> let
            newTime = if T.isPrefixOf "#" l -- If it's a #<time> line
                then parseTime l
                else s.dfsNow
            in passthrough newTime
