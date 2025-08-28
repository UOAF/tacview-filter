module Data.Tacview.MinId (
    minId
) where

import Control.Concurrent.Channel
import Control.Concurrent.STM
import Control.Monad
import Data.HashTable.IO qualified as HM
import Data.IORef
import Data.Set qualified as S
import Data.Tacview

type IdTable = HM.BasicHashTable TacId TacId

data MinIdState = MinIdState {
    idMap :: !IdTable,
    nextId :: !(IORef TacId)
}

minId :: Channel c => c ParsedLine -> c ParsedLine -> IO ()
minId source sink = do
    t <- HM.new
    i <- newIORef 1
    let state = MinIdState t i
    consumeChannel source $ mapLine state >=> atomically . writeChannel' sink

mapLine :: MinIdState -> ParsedLine -> IO ParsedLine
mapLine state (PropLine i p) = do
    mappedId <- lookupOrInsert state i
    mappedProps <- mapM (mapProp state) p
    pure $ PropLine mappedId mappedProps
-- If we see a removal line with an ID we haven't seen before, that's weird,
-- but let's at least make it small.
mapLine state (RemLine i) = RemLine <$> lookupOrInsert state i
mapLine state (EventLine t is p) = do
    mis <- mapM (lookupOrInsert state) $ S.toList is
    pure $ EventLine t (S.fromList mis) p
mapLine _ l = pure l

lookupOrInsert :: MinIdState -> TacId -> IO TacId
lookupOrInsert _ 0 = pure 0
lookupOrInsert state i = HM.lookup state.idMap i >>= \case
    Just m -> pure m
    Nothing -> do
        m <- readIORef state.nextId
        modifyIORef' state.nextId (+ 1)
        HM.insert state.idMap i m
        pure m

-- If we come across a property (say, LockedTarget) referencing a previously-unseen ID,
-- assume we'll run into it soon and make a mapping for it.
mapProp :: MinIdState -> Property -> IO Property
mapProp state (Referencing tid) = Referencing <$> lookupOrInsert state tid
mapProp _ p = pure p
