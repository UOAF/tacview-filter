{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Delta (deltas) where

import Control.Monad
import Control.Concurrent.Channel
import Control.Concurrent.STM
import Control.Exception
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Tacview
import Data.Text (Text)
import Data.Text qualified as T
import Text.Printf

deltas :: HashMap TacId Properties -> Channel (Maybe LineIds, Text) -> Channel Text -> IO ()
deltas !objects source sink = atomically (readChannel source) >>= \case
    Nothing -> pure ()
    Just (i, l) -> deltas' i l objects source sink

deltas'
    :: Maybe LineIds
    -> Text
    -> HashMap TacId Properties
    -> Channel (Maybe LineIds, Text)
    -> Channel Text
    -> IO ()
deltas' mid l !objects source sink = let
    -- Remove the object from the set we're tracking.
    axeIt i = do
        evalWriteChannel sink l
        deltas (HM.delete i objects) source sink
    -- Pass the line through without doing anything.
    passthrough = do
        evalWriteChannel sink l
        deltas objects source sink
    in case mid of
        Just (PropLine p) -> do
            -- Parse the line's properties and see if it's anything we're tracking.
            let props = lineProperties l
                prev = objects HM.!? p
                -- Update the properties we're tracking.
                newProps = case prev of
                    Nothing -> props
                    Just old -> updateProperties old props
                -- Delta-encode the line, omitting properties that haven't changed.
                deltaEncoded = case prev of
                    Nothing -> props
                    Just old -> deltaProperties old props
                toWrite = (T.pack . printf "%x," $ p) <> showProperties deltaEncoded
            -- Don't write if the delta-encoded version is empty (nothing changed).
            unless (HM.null deltaEncoded) $ evalWriteChannel sink toWrite
            deltas (HM.insert p newProps objects) source sink
        Just (RemLine p) -> axeIt p
        -- If it's a "left the area" event, assume its deletion will
        -- come next. We want to write out any last properties before going.
        Just (EventLine es) -> if T.isInfixOf "Event=LeftArea" l
            then assert (HS.size es == 1) $ do
                axeIt (head $ HS.toList es)
            else passthrough
        _other -> passthrough
