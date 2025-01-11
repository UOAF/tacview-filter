module Data.Tacview.Sink (sink) where

import Codec.Archive.Zip
import Codec.Archive.Zip.Internal qualified as ZI
import Conduit
import Control.Concurrent.Channel
import Control.Concurrent.STM
import Data.ByteString qualified as BS
import Data.IORef
import Data.List (isSuffixOf)
import Data.Map.Strict qualified as M
import Data.Tacview (zipExt, txtExt)
import Data.Text (Text)
import Data.Text qualified as T
import System.IO

-- | Write everything out when we're done.
sink
    :: Maybe FilePath
    -> IORef Int
    -> Channel Text
    -> IO ()
sink mfp iow source = do
    sinker <- sinkStream mfp
    let srcC :: ConduitT () BS.ByteString (ResourceT IO) ()
        srcC = repeatMC (liftIO $ atomically (readChannel source))
            .| mapWhileC id
            .| iterMC (const . liftIO $ atomicModifyIORef' iow $ \p -> (p + 1, ()))
            .| unlinesC
            .| encodeUtf8C
    sinker srcC

sinkStream :: Maybe FilePath -> IO (ConduitT () BS.ByteString (ResourceT IO) () -> IO ())
sinkStream = \case
    Nothing -> pure writeStdout
    Just fp -> go where
        go
            | (zipExt `isSuffixOf` fp) = pure $ writeZip fp
            | (txtExt `isSuffixOf` fp) = pure $ writeTxt fp
            | otherwise = fail "expected a .zip.acmi or .txt.acmi file"

writeStdout :: ConduitT () BS.ByteString (ResourceT IO) () -> IO ()
writeStdout src = runConduitRes $ src .| sinkHandle stdout

writeTxt :: FilePath -> ConduitT () BS.ByteString (ResourceT IO) () -> IO ()
writeTxt t src = runConduitRes $ src .| sinkFile tn where
    tn = (T.unpack . T.dropEnd (length txtExt) . T.pack $ t) <> "-filtered" <> txtExt

writeZip :: FilePath -> ConduitT () BS.ByteString (ResourceT IO) () -> IO ()
writeZip z src = do
    let zn = (T.unpack . T.dropEnd (length zipExt) . T.pack $ z) <> "-filtered" <> zipExt
    withBinaryFile zn WriteMode $ \h -> do
        sel <- mkEntrySelector "acmi.txt"
        let eaCompression = M.singleton sel Deflate
            eaEntryComment = M.empty
            eaDeleteComment = M.empty
            eaModTime = M.empty
            eaExtraField = M.empty
            eaDeleteField = M.empty
            eaExtFileAttr = M.empty
            ea = ZI.EditingActions{..}
        (es, des) <- ZI.sinkEntry h sel ZI.GenericOrigin src ea
        let cdmap = M.singleton es des
        ZI.writeCD h (Just "Generated with tacview-filter") cdmap

