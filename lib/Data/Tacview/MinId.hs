module Data.Tacview.MinId (
    minId
) where

import Control.Concurrent.Channel
import Control.Monad
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

minId :: Channel c => c ParsedLine -> c ParsedLine -> IO ()
minId source sink = do
    t <- HM.new
    i <- newIORef 1
    let state = MinIdState t i
    consumeChannel source $ mapLine state >=> evalWriteChannel' sink

mapLine :: MinIdState -> ParsedLine -> IO ParsedLine
mapLine state (PropLine i p) = do
    mappedId <- HM.lookup state.idMap i >>= \case
        Just m -> pure m
        Nothing -> do
            m <- readIORef state.nextId
            modifyIORef' state.nextId (+ 1)
            HM.insert state.idMap i m
            pure m
    mappedProps <- mapM (mapProp state.idMap) p
    pure $ PropLine mappedId mappedProps
mapLine state l@(RemLine i) = HM.lookup state.idMap i >>= \case
    Just m -> do
        HM.delete state.idMap i
        pure $ RemLine m
    -- If we see a removal line with an ID we haven't seen before, that's weird,
    -- but there's not much we can do. Maybe it's a bug in the source file.
    Nothing -> pure l
mapLine state (EventLine t is p) = do
    mis <- mapM (mapId state.idMap) $ S.toList is
    pure $ EventLine t (S.fromList mis) p
mapLine _ l = pure l

-- Same as RemLine - not much we can do if a property references an unknown ID.
mapId :: IdTable -> TacId -> IO TacId
mapId idt i = fromMaybe i <$> HM.lookup idt i

mapProp :: IdTable -> Property -> IO Property
mapProp idt (Referencing tid) = Referencing <$> mapId idt tid
mapProp _ p = pure p
