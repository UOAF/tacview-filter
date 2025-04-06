module Data.Tacview.MinId (
    minId
) where

import Control.Concurrent.Channel
import Control.Concurrent.STM
import Data.HashTable.IO qualified as HM
import Data.IORef
import Data.Set qualified as S
import Data.Maybe
import Data.Tacview

type IdTable = HM.BasicHashTable TacId TacId

data MinIdState = MinIdState {
    idMap :: !IdTable,
    nextId :: !(IORef TacId)
}

minId :: Channel ParsedLine -> Channel ParsedLine -> IO ()
minId source sink = do
    t <- HM.new
    i <- newIORef 1
    minId' (MinIdState t i) source sink

minId' :: MinIdState -> Channel ParsedLine -> Channel ParsedLine -> IO ()
minId' !state source sink = atomically (readChannel source) >>= \case
    Just l -> do
        l' <- mapLine state l
        evalWriteChannel sink l'
        minId' state source sink
    Nothing -> pure ()

mapLine :: MinIdState -> ParsedLine -> IO ParsedLine
mapLine state (PropLine i p) = HM.lookup state.idMap i >>= \case
    Just m -> pure $ PropLine m p
    Nothing -> do
        m <- readIORef state.nextId
        modifyIORef' state.nextId (+ 1)
        HM.insert state.idMap i m
        pure $ PropLine m p
mapLine state l@(RemLine i) = HM.lookup state.idMap i >>= \case
    Just m -> do
        HM.delete state.idMap i
        pure $ RemLine m
    Nothing -> pure l -- Weird, but maybe a bug in the source file.
mapLine state (EventLine t is p) = do
    let mapId :: TacId -> IO TacId
        mapId i = fromMaybe i <$> HM.lookup state.idMap i -- Ditto
    mis <- mapM mapId $ S.toList is
    pure $ EventLine t (S.fromList mis) p
mapLine _ l = pure l
