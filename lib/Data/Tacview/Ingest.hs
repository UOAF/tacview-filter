-- | Common input pipeline for server and filter
--
-- While how we actually delta-encode and filter differs slightly when serving many clients,
-- we read from stdin/files in the same way and want to pass through a common set of filters.
module Data.Tacview.Ingest where

import Control.Concurrent.Channel
import Control.Concurrent.TBCQueue
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Tacview
import Data.Tacview.Ignores
import Data.Tacview.MinId
import Data.Tacview.Source

data ShouldMinifyIds = MinifiedIds | OriginalIds deriving stock (Eq)

ingest
    :: ShouldMinifyIds
    -> Maybe FilePath
    -> IO (TBCQueue ParsedLine -> IO FilteredLines, Maybe SourceLength, SourceProgress)
ingest mini inputPath = do
    (src, mlen, readProgress) <- source inputPath
    let pipe = pipeline (newTBCQueueIO 1024)
        ignore sink = snd <$> pipe src (`filterLines` sink)
        -- Now that we're dealing wiht `Channel ParsedLine -> Channel ParsedLine -> IO ()`
        -- filters, we can chain them in some uniform way.
        -- (For now it's just minifying, but you can imagine further transforms, like relabeling.)
        chain :: (TBCQueue a -> TBCQueue a -> IO ()) -> State (TBCQueue a -> IO FilteredLines) ()
        chain next = modify $ chain' next
        chain' next prev sink = fst <$> pipe prev (`next` sink)
        finalChain = flip execState ignore $ do
            when (mini == MinifiedIds) $ chain minId
            -- ...and so on for more filters.

    pure (finalChain, mlen, readProgress)
