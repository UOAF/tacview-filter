module Data.Tacview.Sink (sink, sinkZip) where

import Codec.Archive.Zip
import Codec.Archive.Zip.Internal qualified as ZI
import Conduit
import Control.Concurrent.Channel
import Control.Concurrent.STM
import Data.ByteString qualified as BS
import Data.IORef
import Data.List (isSuffixOf)
import Data.Map.Strict qualified as M
import Data.Tacview.Annotate
import Data.Tacview (ParsedLine, showLine, zipExt, txtExt)
import Data.Text qualified as T
import System.IO

-- | Write everything out when we're done.
sink :: Channel c => Maybe FilePath -> IO (c ParsedLine -> IO (), IORef Integer)
sink mfp = do
    iow <- newIORef 0
    to <- sinkStream mfp
    let srcC source = repeatMC (liftIO $ atomically (readChannel source))
            .| mapWhileC id
            .| mapC showLine
            .| iterMC (const . liftIO $ atomicModifyIORef' iow $ \p -> (p + 1, ()))
            .| unlinesC
            .| encodeUtf8C
        sinker src = to $ srcC src
    pure (sinker, iow)

-- | Writes already-printed lines to a ZIP file without progress; useful for the server.
sinkZip :: Channel c => FilePath -> IO (c T.Text -> IO ())
sinkZip fp = do
    let srcC source = repeatMC (liftIO $ atomically (readChannel source))
            .| mapWhileC id
            .| unlinesC
            .| encodeUtf8C
        sinker src = writeZip' fp $ srcC src
    pure sinker

sinkStream :: Maybe FilePath -> IO (ConduitT () BS.ByteString (ResourceT IO) () -> IO ())
sinkStream = \case
    Nothing -> pure writeStdout
    Just fp -> go where
        go
            | (zipExt `isSuffixOf` fp) = pure $ writeZip fp
            | (txtExt `isSuffixOf` fp) = pure $ writeTxt fp
            | otherwise = fail "expected a .zip.acmi or .txt.acmi file"

writeStdout :: ConduitT () BS.ByteString (ResourceT IO) () -> IO ()
writeStdout src = whileIO "while writing to stdout" $ runConduitRes $ src .| sinkHandle stdout

writeTxt :: FilePath -> ConduitT () BS.ByteString (ResourceT IO) () -> IO ()
writeTxt t src = whileIO ("while writing to " <> tn) $ runConduitRes $ src .| sinkFile tn where
    tn = (T.unpack . T.dropEnd (length txtExt) . T.pack $ t) <> "-filtered" <> txtExt

writeZip :: FilePath -> ConduitT () BS.ByteString (ResourceT IO) () -> IO ()
writeZip z = writeZip' zn where
    zn = (T.unpack . T.dropEnd (length zipExt) . T.pack $ z) <> "-filtered" <> zipExt

writeZip' :: FilePath -> ConduitT () BS.ByteString (ResourceT IO) () -> IO ()
writeZip' z src = whileIO ("while writing to " <> z) $ do
    withBinaryFile z WriteMode $ \h -> do
        sel <- mkEntrySelector "acmi.txt"
        let eaCompression = M.singleton sel Deflate
            eaEntryComment = M.empty
            eaDeleteComment = M.empty
            eaModTime = M.empty
            eaExtraField = M.empty
            eaDeleteField = M.empty
            eaExtFileAttr = M.empty
            ea = ZI.EditingActions{..}
        des <- ZI.sinkEntry h sel ZI.GenericOrigin src ea
        let cdmap = M.singleton sel des
        ZI.writeCD h (Just "Generated with tacview-filter") cdmap

