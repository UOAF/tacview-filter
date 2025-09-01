module Data.Tacview.Source (
    source,
    SourceLength (..),
    SourceProgress(..)
) where

import Codec.Archive.Zip
import Conduit
import Control.Concurrent.Channel
import Control.Concurrent.STM
import Control.Monad
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.IORef
import Data.List (isSuffixOf)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Tacview (zipExt, txtExt)
import Data.Tacview.Annotate
import Data.Text (Text)
import System.Directory (getFileSize)
import System.IO

type ByteConduit = ConduitT () BS.ByteString (ResourceT IO) ()

newtype SourceLength = SourceLength { unLength :: Integer }

sourceC :: Maybe FilePath -> IO (ByteConduit, Maybe SourceLength)
sourceC = \case
    Nothing -> pure (readStdin, Nothing)
    Just fp -> second Just <$> go where
        go
            | (zipExt `isSuffixOf` fp) = readZip fp
            | (txtExt `isSuffixOf` fp) = readTxt fp
            | otherwise = fail "expected a .zip.acmi or .txt.acmi file"

readStdin :: ByteConduit
readStdin = sourceHandle stdin

readTxt :: FilePath -> IO (ByteConduit, SourceLength)
readTxt fp = do
    len <- getFileSize fp
    pure (sourceFile fp, SourceLength len)

readZip :: FilePath -> IO (ByteConduit, SourceLength)
readZip z = withArchive z $ do
    e <- getEntries
    when (M.null e) $ error "empty ZIP archive!"
    let (sel, des) = head $ M.toList e
    src <- getEntrySource sel
    pure (src, SourceLength $ fromIntegral des.edUncompressedSize)

data SourceProgress = SourceProgress {
    bytes:: IORef Integer,
    lines :: IORef Integer
}

source :: Channel c
    => Maybe FilePath
    -> IO (c Text -> IO (), Maybe SourceLength, SourceProgress)
source mfp = do
    (srcC, len) <- sourceC mfp
    prog <- SourceProgress <$> newIORef 0 <*> newIORef 0
    let countBytes b = liftIO $
            atomicModifyIORef' prog.bytes $ \p -> (p + fromIntegral (BS.length b), ())
        go c l = do
            atomically $ writeChannel' c l
            atomicModifyIORef' prog.lines $ \p -> (p + 1, ())
        run c = whileIO ("while reading " <> fromMaybe "stdin" mfp) $ runConduitRes $
            srcC .| iterMC countBytes .| decodeUtf8C .| linesUnboundedC .| mapM_C (liftIO . go c)
    pure (run, len, prog)
