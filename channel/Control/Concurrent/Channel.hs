{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Control.Concurrent.Channel(
    Channel,
    newChannel,
    newChannelIO,
    readChannel,
    writeChannel,
    closeChannel,
    pipeline
) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Numeric.Natural

data Channel a = Channel {
    q :: TBQueue a,
    closed :: TVar Bool
}

newChannel :: Natural -> STM (Channel a)
newChannel n = do
    q <- newTBQueue n
    closed <- newTVar False
    pure $ Channel {..}

newChannelIO :: Natural -> IO (Channel a)
newChannelIO = atomically . newChannel

readChannel :: Channel a -> STM (Maybe a)
readChannel c = do
    mv <- tryReadTBQueue c.q
    case mv of
        Just v -> pure $ Just v
        Nothing -> do
            wasClosed <- readTVar c.closed
            if wasClosed
                then pure Nothing
                else retry

writeChannel :: Channel a -> a -> STM ()
writeChannel c !v = do
    wasClosed <- readTVar c.closed
    if wasClosed
        then error "write to closed channel"
        else writeTBQueue c.q v

closeChannel :: Channel a -> STM ()
closeChannel c = writeTVar c.closed True

pipeline :: (Channel a -> IO ()) -> (Channel a -> IO ()) -> IO ()
pipeline producer consumer = do
    c <- newChannelIO 64 -- Arbitrary
    let producer' = producer c `finally` atomically (closeChannel c)
    concurrently_ producer' (consumer c)
