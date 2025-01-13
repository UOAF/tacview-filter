{-# LANGUAGE StrictData #-}

-- | Common delta-encoding machinery for the filter and server
module Data.Tacview.Delta where

import Data.HashMap.Strict qualified as HM
import Data.Tacview
import Data.Text (Text)
import Data.Text qualified as T

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
        in (ObjectState{..}, Just $ buildLine i props)
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
