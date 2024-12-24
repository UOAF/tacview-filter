{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Main where

import Conduit
import Codec.Archive.Zip.Internal qualified as ZI
import Codec.Archive.Zip
import Data.Map.Strict qualified as M
import System.IO

main :: IO ()
main = do
    -- lol Windows
    hSetNewlineMode stdin noNewlineTranslation
    hSetNewlineMode stdout noNewlineTranslation
    hSetNewlineMode stderr noNewlineTranslation

    let src = stdinC .| decodeUtf8C .| linesUnboundedC
    withBinaryFile "out.zip" WriteMode $ \h -> do
        sel <- mkEntrySelector "out.zip.acmi"
        let eaCompression = M.singleton sel Deflate
            eaEntryComment = M.empty
            eaDeleteComment = M.empty
            eaModTime = M.empty
            eaExtraField = M.empty
            eaDeleteField = M.empty
            eaExtFileAttr = M.empty
            ea = ZI.EditingActions{..}
            pipe = src .| unlinesC .| encodeUtf8C
        (es, des) <- ZI.sinkEntry h sel ZI.GenericOrigin pipe ea
        let cdmap = M.singleton es des
        ZI.writeCD h Nothing cdmap
