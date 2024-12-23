{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Main where

import Conduit
import Codec.Archive.Zip
import System.IO

main :: IO ()
main = do
    -- lol Windows
    hSetNewlineMode stdin noNewlineTranslation
    hSetNewlineMode stdout noNewlineTranslation
    hSetNewlineMode stderr noNewlineTranslation

    let src = stdinC .| decodeUtf8C .| linesUnboundedC
    createArchive "out.zip" $ do
        sel <- mkEntrySelector "out.txt"
        let pipe = src .| unlinesC .| encodeUtf8C
        sinkEntry Deflate pipe sel
