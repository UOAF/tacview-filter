module Data.Tacview.MinId (
    minId
) where

import Control.Concurrent.Channel
import Control.Concurrent.STM
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as S
import Data.Maybe
import Data.Tacview

data MinIdState = MinIdState {
    idMap :: !(HashMap TacId TacId),
    nextId :: !TacId
}

minId :: Channel ParsedLine -> Channel ParsedLine -> IO ()
minId source sink = minId' (MinIdState mempty 1) source sink

minId' :: MinIdState -> Channel ParsedLine -> Channel ParsedLine -> IO ()
minId' !state source sink = atomically (readChannel source) >>= \case
    Just l -> do
        let (l', next) = mapLine state l
        evalWriteChannel sink l'
        minId' next source sink
    Nothing -> pure ()

mapLine :: MinIdState -> ParsedLine -> (ParsedLine, MinIdState)
mapLine state (PropLine i p) = case state.idMap HM.!? i of
    Just m -> (PropLine m p, state)
    Nothing -> (PropLine m p, newState) where
        m = state.nextId
        newState = state { idMap = HM.insert i m state.idMap, nextId = state.nextId + 1 }
mapLine state l@(RemLine i) = case state.idMap HM.!? i of
    Just m -> (RemLine m, state { idMap = HM.delete i state.idMap })
    Nothing -> (l, state) -- Weird, but maybe a bug in the source file.
mapLine state (EventLine t is p) = (EventLine t mis p, state) where
    mis = S.map mapId is
    mapId :: TacId -> TacId
    mapId i = fromMaybe i $ state.idMap HM.!? i -- Ditto
mapLine s l = (l, s)

