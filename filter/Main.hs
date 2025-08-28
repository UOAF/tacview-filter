module Main where

import Control.Concurrent.Async
import Control.Concurrent hiding (yield)
import Control.Concurrent.Channel
import Control.Concurrent.TBCQueue
import Control.Exception
import Control.Monad
import Data.Function (fix)
import Data.IORef
import Data.Maybe
import Data.Ratio
import Data.Tacview.Annotate
import Data.Tacview.Ignores as Ignores
import Data.Tacview.MinId
import Data.Tacview.Sink qualified as Tacview
import Data.Tacview.Source (SourceProgress(..))
import Data.Tacview.Source qualified as Tacview
import Data.Text qualified as T
import Data.Text.IO qualified as T
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
        ignore sink = pipe src (`filterLines` sink)
        thenDeltas sink = pipe ignore (`deltas` sink)
        thenMinId sink = pipe thenDeltas (`minId` sink)
        filterPipeline = whileIO ("in " <> fromMaybe "stdin" zipInput) $ pipe thenMinId dst
        prog = if noProgress
            then forever $ threadDelay maxBound
            else progress mlen readProgress linesWritten

    runnit <- race filterPipeline prog

    -- Gather up all our stats, placed in newtypes for easier readability here.
    let (((((), FilteredLines f), ()), ()), ()) = case runnit of
            Left l -> l
            Right () -> error "absurd: progress should run forever"

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

progress :: Maybe Integer -> Tacview.SourceProgress -> IORef Integer -> IO ()
progress mlen i o = progress' mlen i o 0 `onException` hPutStr stderr "\n\n"

progress' :: Maybe Integer -> Tacview.SourceProgress -> IORef Integer -> Int -> IO ()
progress' mlen i o = fix $ \loop !n -> do
    i' <- readIORef i.lines
    if i' == 0
        then do
            hPutStr stderr "waiting for input..."
            let waitForInput = do
                    -- Polling is bad but I'll take it instead of busting out STM just yet.
                    threadDelay 100000 -- 20 FPS
                    i'' <- readIORef i.lines
                    if i'' == 0 then waitForInput else loop n
            waitForInput


        else do
            b <- readIORef i.bytes
            o' <- readIORef o
            let cc = clearFromCursorToLineBeginningCode
                donePercent = case mlen of
                    Just len -> ", " <> show (round $ b % len * 100) <> "% done"
                    Nothing -> ""
                compressedPercent = show $ round $ (o' % max 1 i') * 100
            T.hPutStr stderr . T.pack $ mconcat [
                cc,
                "\r",
                showChar (spinny n) " ",
                show o',
                "/",
                show i',
                " lines out/in (",
                compressedPercent,
                "% original size",
                donePercent,
                ")"
                ]
            threadDelay 100000 -- 20 FPS
            loop (n + 1)

spinny :: Int -> Char
spinny n = case n `mod` 4 of
    0 -> '|'
    1 -> '/'
    2 -> '-'
    3 -> '\\'
    _ -> undefined
