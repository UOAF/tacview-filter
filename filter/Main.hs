module Main where

import Control.Concurrent.Async
import Control.Concurrent hiding (yield)
import Control.Concurrent.Channel
import Control.Concurrent.TBCQueue
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Ratio
import Data.Tacview.Annotate
import Data.Tacview.Ignores as Ignores
import Data.Tacview.MinId
import Data.Tacview.Sink qualified as Tacview
import Data.Tacview.Source (SourceProgress(..))
import Data.Tacview.Source qualified as Tacview
import Data.Void
import Numeric
import Options.Applicative
import System.Clock.Seconds
import System.Console.ANSI
import System.IO

import Delta

data Args = Args {
    zipInput :: Maybe FilePath,
    noProgress :: Bool
}

parseArgs :: Parser Args
parseArgs = Args <$> parseZipIn <*> parseNoProgress where
    parseZipIn = optional . strArgument $ mconcat [
        help "ACMI to filter. Otherwise reads from stdin and writes to stdout",
        metavar "recording.zip.acmi"
        ]
    parseNoProgress = switch $ mconcat [
        long "no-progress",
        help "Suppress progress printed to stderr"
        ]


main :: IO ()
main = do
    -- lol Windows
    hSetNewlineMode stdin noNewlineTranslation
    hSetNewlineMode stdout noNewlineTranslation
    hSetNewlineMode stderr noNewlineTranslation

    -- lol IO
    hSetBuffering stderr $ BlockBuffering Nothing
    hSetBuffering stdin $ BlockBuffering Nothing
    hSetBuffering stdout $ BlockBuffering Nothing

    let parser = customExecParser (prefs showHelpOnError) parseInfo
        parseInfo = info (parseArgs <**> helper) $
            progDesc "Ensmallens ACMI files"
    parser >>= runFilter

runFilter :: Args -> IO ()
runFilter Args{..} = do
    start <- getTime Monotonic

    -- Hoare Was Right.
    -- The program is pipelined (see Data.Concurrency.Channel):
    -- Major steps are run in their own task.
    -- They filters or modify lines, then pass the remainders to the next stage of the pipe.
    -- This parallelizes trivially; the runtime runs each task in a free thread.
    (src, mlen, readProgress) <- Tacview.source zipInput
    (dst, linesWritten) <- Tacview.sink zipInput
    let pipe = pipeline (newTBCQueueIO 1024)
        ignore sink = snd <$> pipe src (`filterLines` sink)
        thenMinId sink = fst <$> pipe ignore (`minId` sink)
        thenDeltas sink = fst <$> pipe thenMinId (`deltas` sink)
        filterPipeline = whileIO ("in " <> fromMaybe "stdin" zipInput) $
            fst <$> pipe thenDeltas dst
        prog = if noProgress
            then forever $ threadDelay maxBound
            else progress mlen readProgress linesWritten

    runnit <- race filterPipeline prog
    printProgress mlen readProgress linesWritten Nothing -- Print 100%

    -- Gather up our stats, placed in newtypes for easier readability here.
    let (FilteredLines f) = case runnit of
            Left l -> l
            Right r -> absurd r

    end <- getTime Monotonic
    let dt = end - start
        dts = showFFloat (Just 2) (realToFrac dt :: Double) ""
    unless noProgress $ do
        -- Clear any progress bar
        hClearFromCursorToLineBeginning stderr
        hCursorDownLine stderr 0
    hPutStrLn stderr $ "in " <> dts <> " seconds"
    hPutStrLn stderr $ show f <> " lines ignored"
    -- Hmm: https://gitlab.haskell.org/ghc/ghc/-/issues/22468
    -- https://github.com/haskell/core-libraries-committee/issues/112
    i <- readIORef readProgress.lines
    o <- readIORef linesWritten
    hPutStrLn stderr $ mconcat [
        show o,
        "/",
        show i,
        " total lines out/in (",
        showFFloat (Just 2) (realToFrac $ o % i * 100) "% original size)"
        ]
    let perSec = fromIntegral i / toRational dt
    hPutStrLn stderr $ show (round perSec :: Integer) <> " lines/second"

progress :: Maybe Integer -> Tacview.SourceProgress -> IORef Integer -> IO Void
progress mlen i o = progress' mlen i o `onException` hPutStr stderr "\n\n"

progress' :: Maybe Integer -> Tacview.SourceProgress -> IORef Integer -> IO Void
progress' mlen i o = do
    hPutStr stderr "waiting for input..."
    hFlush stderr
    let waitForInput = do
            -- Polling is bad but I'll take it instead of busting out STM just yet.
            threadDelay 100000
            i' <- readIORef i.lines
            when (i' == 0) waitForInput
    waitForInput
    let go !n = do
            printProgress mlen i o (Just n)
            threadDelay 100000
            go (n + 1)

    go 0 where

printProgress :: Maybe Integer -> Tacview.SourceProgress -> IORef Integer -> Maybe Int -> IO ()
printProgress mlen i o mn = do
    i' <- readIORef i.lines
    b <- readIORef i.bytes
    o' <- readIORef o
    let cc = clearFromCursorToLineBeginningCode
        donePercent = case mlen of
            Just len -> ", " <> show (round $ b % len * 100) <> "% done"
            Nothing -> ""
        compressedPercent = show $ round $ (o' % max 1 i') * 100
    hPutStr stderr $ mconcat [
        cc,
        "\r",
        showChar (spinny mn) " ",
        show o',
        "/",
        show i',
        " lines out/in (",
        compressedPercent,
        "% original size",
        donePercent,
        ")"
        ]
    hFlush stderr

spinny :: Maybe Int -> Char
spinny Nothing = ' '
spinny (Just n) = case n `mod` 4 of
    0 -> '|'
    1 -> '/'
    2 -> '-'
    3 -> '\\'
    _ -> undefined
