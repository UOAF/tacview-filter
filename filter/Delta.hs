{-# LANGUAGE StrictData #-}

module Delta (deltas) where

import Control.Concurrent.Channel
import Control.Concurrent.STM
import Control.Exception
import Data.Bifunctor
import Data.HashTable.IO qualified as HM
import Data.IORef
import Data.Set qualified as S
import Data.Maybe
import Data.Tacview
import Data.Tacview.Delta

type StateTable = HM.BasicHashTable TacId ObjectState

data DeltaFilterState = DeltaFilterState {
    liveObjects :: StateTable,
    now :: IORef Double,
    lastWrittenTime :: IORef Double
}

startState :: IO DeltaFilterState
startState = do
    t <- HM.new
    n <- newIORef 0.0
    l <- newIORef 0.0
    pure $ DeltaFilterState t n l

writeOut :: Channel ParsedLine -> [ParsedLine] -> IO ()
writeOut c ts = mapM_ (evalWriteChannel c) ts

deltas :: Channel ParsedLine -> Channel ParsedLine -> IO ()
deltas source sink = do
    s <- startState
    deltas' s source sink

deltas'
    :: DeltaFilterState
    -> Channel ParsedLine
    -> Channel ParsedLine -> IO ()
deltas' !dfs source sink = atomically (readChannel source) >>= \case
    -- Delta-encode all remaining objects on the way out
    -- so we don't drop any last-second changes.
    Nothing -> do
        let closeLine :: TacId -> ObjectState -> Maybe ParsedLine
            closeLine i s = PropLine i <$> closeOut s
        los <- HM.toList dfs.liveObjects
        let allClosed = catMaybes $ uncurry closeLine <$> los
        writeOut sink allClosed
    Just p -> do
        newLines <- processLine dfs p
        writeOut sink $ catMaybes newLines
        deltas' dfs source sink

processLine
    :: DeltaFilterState
    -> ParsedLine
    -> IO [Maybe ParsedLine]
processLine !dfs p = let
    -- Helper to write a timestamp when we need a new one.
    writeTimestamp :: IO (Maybe ParsedLine)
    writeTimestamp = do
        now <- readIORef dfs.now
        lastTime <- readIORef dfs.lastWrittenTime
        if now /= lastTime
            then do
                writeIORef dfs.lastWrittenTime now
                pure . Just $ TimeLine now
            else pure Nothing
    -- Helper to remove the object from the set we're tracking, taking the ID
    axeIt :: TacId -> IO [Maybe ParsedLine]
    axeIt x = do
        maybeTime <- writeTimestamp
        -- We might not have written in in a bit,
        -- so make sure its last known state goes out.
        o <- HM.lookup dfs.liveObjects x
        let co = o >>= closeOut
        HM.delete dfs.liveObjects x
        pure [PropLine x <$> co, maybeTime, Just p]
    -- Helper to pass the line through without doing anything, taking the time.
    passthrough :: IO [Maybe ParsedLine]
    passthrough = do
        maybeTime <- writeTimestamp
        pure [maybeTime, Just p]
    in case p of
        PropLine pid props -> do
            now <- readIORef dfs.now
            let upfun :: Maybe ObjectState -> (Maybe ObjectState, Maybe Properties)
                upfun prev = first Just $ updateObject prev now props
            -- Update the properties we're tracking.
            deltaLine <- HM.mutate dfs.liveObjects pid upfun
            -- If we have a delta line, write some stuff...
            case deltaLine of
                Just dl -> do
                    maybeTime <- writeTimestamp
                    pure [maybeTime, Just $ PropLine pid dl]
                Nothing -> pure [] -- Otherwise just update liveObjects
        RemLine pid -> axeIt pid
        -- If it's a "left the area" event, assume its deletion will
        -- come next. We want to write out any last properties before going.
        EventLine t es _ -> if t == "LeftArea"
            then assert (S.size es == 1) $ axeIt (head $ S.toList es)
            else passthrough
        -- If it's a #<time> line, note the new time but dont write.
        TimeLine t -> do
            writeIORef dfs.now t
            pure []
        -- We don't do anything with global config lines.
        OtherLine _ -> passthrough
