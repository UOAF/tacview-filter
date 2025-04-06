module Main where

import Control.Concurrent.Async
import Control.Concurrent hiding (yield)
import Control.Concurrent.Channel
import Control.Monad
import Data.Function (fix)
import Data.IORef
import Data.Tacview.Ignores as Ignores
import Data.Tacview.MinId
import Data.Tacview.Sink qualified as Tacview
import Data.Tacview.Source qualified as Tacview
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.Float
import Options.Applicative
import System.Clock.Seconds
import System.Console.ANSI
import System.IO
import Text.Printf

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
    linesRead <- newIORef 0
    linesWritten <- newIORef 0
    let src = Tacview.source zipInput linesRead
    let dst = Tacview.sink zipInput linesWritten
    let ignore sink = pipeline
            src
            (\source -> filterLines Ignores.startState source sink)
        thenDeltas sink = pipeline ignore (\source -> deltas source sink)
        thenMinId sink = pipeline thenDeltas (\source -> minId source sink)
        filterPipeline = pipeline thenMinId dst
        prog = if noProgress
            then forever $ threadDelay maxBound
            else progress linesRead linesWritten

    runnit <- race filterPipeline prog

    -- Gather up all our stats, placed in newtypes for easier readability here.
    let (((((), FilteredLines f), ()), ()), ()) = case runnit of
            Left l -> l
            Right () -> error "absurd: progress should run forever"

    end <- getTime Monotonic
    let dt = end - start
        dts = printf "%.2f" (realToFrac dt :: Double)
    unless noProgress $ do
        -- Clear any progress bar
        hClearFromCursorToLineBeginning stderr
        hCursorDownLine stderr 0
    hPutStrLn stderr $ "in " <> dts <> " seconds"
    hPutStrLn stderr $ show f <> " lines ignored"
    -- Hmm: https://gitlab.haskell.org/ghc/ghc/-/issues/22468
    -- https://github.com/haskell/core-libraries-committee/issues/112
    i <- readIORef linesRead
    o <- readIORef linesWritten
    hPutStrLn stderr $ percentage o i <> " total lines in/out"
    let perSec = fromIntegral i / toRational dt
    hPutStrLn stderr $ show (round perSec :: Integer) <> " lines/second"

-- | Express n/d as both that ratio and a percentage
percentage :: Int -> Int -> String
percentage n d = let p = int2Double n / int2Double d * 100 :: Double
    in printf "%d/%d (%.2f%%)" n d p

progress :: IORef Int -> IORef Int -> IO ()
progress i o = progress' i o 0

progress' :: IORef Int -> IORef Int -> Int -> IO ()
progress' i o = fix $ \loop n -> do
    i' <- readIORef i
    if i' == 0
        then do
            hPutStr stderr "waiting for input on stdin..."
            let waitForInput = do
                    -- Polling is bad but I'll take it instead of busting out STM just yet.
                    threadDelay 100000 -- 20 FPS
                    i'' <- readIORef i
                    if i'' == 0 then waitForInput else loop n
            waitForInput


        else do
            o' <- readIORef o
            let p = int2Double o' / int2Double (max 1 i') * 100
                cc = clearFromCursorToLineBeginningCode
            T.hPutStr stderr . T.pack $
                printf "%v\r%c %v lines in / %v out (%.0f%%)" cc (spinny n) i' o' p
            threadDelay 100000 -- 20 FPS
            loop (n + 1)

spinny :: Int -> Char
spinny n = case n `mod` 4 of
    0 -> '|'
    1 -> '/'
    2 -> '-'
    3 -> '\\'
    _ -> undefined
