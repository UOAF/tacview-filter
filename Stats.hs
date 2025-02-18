{-# LANGUAGE StrictData #-}
module Main where

import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import Control.Concurrent.Channel
import Control.Concurrent.STM
import Data.Function (fix)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IORef
import Data.Tacview
import Data.Tacview.Source qualified as Tacview
import Data.Text (Text)
import Numeric
import Options.Applicative
import System.Clock.Seconds
import System.Console.ANSI
import System.IO

newtype Args = Args {
    zipInput :: Maybe FilePath
}

parseArgs :: Parser Args
parseArgs = Args <$> parseZipIn where
    parseZipIn = optional . strArgument $ mconcat [
        help "ACMI to replay. Otherwise reads from stdin and writes to stdout",
        metavar "recording.zip.acmi"
        ]

main :: IO ()
main = do
    -- lol Windows
    hSetNewlineMode stdin noNewlineTranslation
    hSetNewlineMode stdout noNewlineTranslation
    hSetNewlineMode stderr noNewlineTranslation

    -- lol latency
    hSetBuffering stdout LineBuffering

    let parser = customExecParser (prefs showHelpOnError) parseInfo
        parseInfo = info (parseArgs <**> helper) $
            progDesc "Replays ACMI files at the rate they were recorded"
    parser >>= run

run :: Args -> IO ()
run Args{..} = do
    start <- getTime Monotonic
    linesRead <- newIORef 0
    let src = Tacview.source zipInput linesRead
        go = snd <$> pipeline src (runStats startingState)
    stats <- race (progress linesRead) go >>= \case
        Left () -> error "absurd: progress loop ended"
        Right s -> pure s
    end <- getTime Monotonic
    let dt = end - start
        dts = showFFloat (Just 2) (realToFrac dt :: Double) ""
    hPutStrLn stderr $ "\nin " <> dts <> " seconds"
    hPutStrLn stderr $ "read " <> show stats.currentTime <> " seconds"

-- Append for each dt between objects - i.e., when `liveObjects !? o` is `Just prev`.
data TypeStats = TypeStats {
    minDelta :: Double,
    maxDelta :: Double,
    totalDelta :: Double, -- cumulative delta, divided by...
    numDeltas :: Integer  -- ...to get the average
}

initStats :: Double -> TypeStats
initStats dt = TypeStats{..} where
    minDelta = dt
    maxDelta = dt
    totalDelta = dt
    numDeltas = 1

appendDelta :: TypeStats -> Double -> TypeStats
appendDelta p dt = TypeStats{..} where
    minDelta = min p.minDelta dt
    maxDelta = max p.maxDelta dt
    totalDelta = p.totalDelta + dt
    numDeltas = p.numDeltas + 1

newtype ObjectState = ObjectState {
    lastSeen :: Double
}

data StatState = StatState {
    currentTime :: Double,
    liveObjects :: HashMap TacId ObjectState,
    typeStats :: HashMap Text TypeStats
}

startingState :: StatState
startingState = StatState 0.0 mempty mempty

runStats :: StatState -> Channel Text -> IO StatState
runStats !s lc = atomically (readChannel lc) >>= \case
    Just l -> runStats (update s l) lc
    Nothing -> pure s

update :: StatState -> Text -> StatState
update s l = case parseLine l of
    TimeLine t -> s {currentTime = t}
    PropLine tid _props -> s { liveObjects = added } where
        added = HM.insert tid nos s.liveObjects
        nos = ObjectState { lastSeen = s.currentTime }
    RemLine tid -> s { liveObjects = axed} where
        axed = HM.delete tid s.liveObjects
    _ -> s

progress :: IORef Int -> IO ()
progress i = progress' i 0

progress' :: IORef Int -> Int -> IO ()
progress' i = fix $ \loop n -> do
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
            let cc = clearFromCursorToLineBeginningCode
            hPutStr stderr $
                mconcat [ cc, "\r", [spinny n], " Read ", show i', " lines"]
            threadDelay 100000 -- 20 FPS
            loop (n + 1)

spinny :: Int -> Char
spinny n = case n `mod` 4 of
    0 -> '|'
    1 -> '/'
    2 -> '-'
    3 -> '\\'
    _ -> undefined
