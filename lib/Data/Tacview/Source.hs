module Data.Tacview.Source (
    source
) where

import Codec.Archive.Zip
import Conduit
import Control.Concurrent.Channel
import Control.Monad
import Data.ByteString qualified as BS
import Data.IORef
import Data.List (isSuffixOf)
import Data.Map.Strict qualified as M
import Data.Tacview (zipExt, txtExt)
import Data.Text (Text)
import System.IO

sourceC :: Maybe FilePath -> IO (ConduitT () BS.ByteString (ResourceT IO) ())
sourceC = \case
    Nothing -> pure readStdin
    Just fp -> go where
        go
            | (zipExt `isSuffixOf` fp) = readZip fp
            | (txtExt `isSuffixOf` fp) = pure $ readTxt fp
            | otherwise = fail "expected a .zip.acmi or .txt.acmi file"

readStdin :: ConduitT () BS.ByteString (ResourceT IO) ()
readStdin = sourceHandle stdin

readTxt :: FilePath -> ConduitT () BS.ByteString (ResourceT IO) ()
readTxt = sourceFile

readZip :: FilePath -> IO (ConduitT () BS.ByteString (ResourceT IO) ())
readZip z = withArchive z $ do
    e <- getEntries
    when (M.null e) $ error "empty ZIP archive!"
    let sel = head $ M.keys e
    getEntrySource sel

source
    :: Maybe FilePath
    -> IORef Int
    -> Channel Text
    -> IO ()
source mfp ior c = do
    srcC <- sourceC mfp
    let go l = do
            evalWriteChannel c l
            atomicModifyIORef' ior $ \p -> (p + 1, ())
    runConduitRes $
        srcC .| decodeUtf8C .| linesUnboundedC .| mapM_C (liftIO . go)
