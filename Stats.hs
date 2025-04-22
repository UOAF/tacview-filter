{-# LANGUAGE StrictData #-}
module Main where

import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import Control.Concurrent.Channel
import Control.Concurrent.TBCQueue
import Control.Exception
import Data.Function (fix)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IORef
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Ord (Down(..))
import Data.Ratio
import Data.Tacview
import Data.Tacview.Source qualified as Tacview
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
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
    (src, mlen, readProgress) <- Tacview.source zipInput
    let go = snd <$> pipeline (newTBCQueueIO 1024) src runStats
    stats <- race (progress mlen readProgress) go >>= \case
        Left () -> error "absurd: progress loop ended"
        Right s -> pure s
    end <- getTime Monotonic
    let dt = end - start
        dts = showFFloat (Just 2) (realToFrac dt :: Double) ""
    hPutStrLn stderr $ "\nin " <> dts <> " seconds"
    putStrLn "Update rates by type in Hz:"
    M.foldMapWithKey (\k v -> putStrLn $ showDeltaStats k v) stats.deltaStats
    putStrLn ""
    printCounts stats.counts

-- Append for each dt between objects - i.e., when `liveObjects !? o` is `Just prev`.
data DeltaStats = DeltaStats {
    minDelta :: Double,
    maxDelta :: Double,
    totalDelta :: Double, -- cumulative delta, divided by...
    numDeltas :: Int -- ...to get the average
}

initStats :: Double -> DeltaStats
initStats dt = assert (dt >= 0) DeltaStats{..} where
    minDelta = dt
    maxDelta = dt
    totalDelta = dt
    numDeltas = 1

appendDelta :: Double -> DeltaStats -> DeltaStats
appendDelta dt p = assert (dt >= 0) DeltaStats{..} where
    minDelta = min p.minDelta dt
    maxDelta = max p.maxDelta dt
    totalDelta = p.totalDelta + dt
    numDeltas = p.numDeltas + 1

showDeltaStats :: Text -> DeltaStats -> String
showDeltaStats t s = mconcat [
    T.unpack t,
    ": mean ",
    showHz (s.totalDelta / fromIntegral s.numDeltas),
    ", min ",
    showHz s.maxDelta, -- NB: we're taking the inverse below to get frequency from period.
    ", max ",
    showHz s.minDelta
    ] where
    showHz :: Double -> String
    showHz 0 = "∞"
    showHz d = showFFloat (Just 3) (1 / d) ""

printCounts :: HashMap Text Int -> IO ()
printCounts cs = do
    let cs' = HM.toList cs
        total = sum $ fmap snd cs'
        scs = sortOn (Down . snd) cs'
    putStrLn $ "Counts (" <> show total <> " lines total):"
    let percentage :: Int -> Int
        percentage v = round $ fromIntegral v / (fromIntegral total :: Double) * 100
        line (k, v) = show (percentage v) <> "% " <> T.unpack k <> " (" <> show v <> " lines)"
    mapM_ putStrLn $ fmap line scs

data ObjectState = ObjectState {
    lastSeen :: Double,
    tacType :: Maybe Text
}

data StatState = StatState {
    currentTime :: Double,
    liveObjects :: HashMap TacId ObjectState,
    deltaStats :: Map Text DeltaStats,
    counts :: HashMap Text Int
}

startingState :: StatState
startingState = StatState 0.0 mempty mempty mempty

runStats :: Channel c => c Text -> IO StatState
runStats lc = stateConsumeChannel lc startingState $ \ !s l -> pure (update s l)

update :: StatState -> Text -> StatState
update s l = case parseLine l of
    TimeLine t -> s {currentTime = t}
    PropLine tid props -> newState where
        prev = s.liveObjects HM.!? tid
        newObj = updateObjectState s.currentTime props prev
        newObjs = HM.insert tid newObj s.liveObjects
        newType = typeOf newObj
        od = objectDelta prev s.currentTime
        newStats = updateTypeStats od newType s.deltaStats
        newCounts = HM.insertWith (+) newType 1 s.counts
        newState = s {
            liveObjects = newObjs,
            deltaStats = newStats,
            counts = newCounts
        }
    RemLine tid -> s { liveObjects = axed} where
        axed = HM.delete tid s.liveObjects
    _ -> s

unprop :: Property -> Text
unprop (Property p) = p
unprop (Position _) = error "absurd: position as Type"
unprop (Referencing _) = error "absurd: Focused/LockedTarget as Type"

updateObjectState :: Double -> Properties -> Maybe ObjectState -> ObjectState
updateObjectState now props Nothing = ObjectState{..} where
    lastSeen = now
    tacType = unprop <$> props HM.!? "Type"
updateObjectState now props (Just prev) = ObjectState{..} where
    lastSeen = now
    tacType = prev.tacType <|> (unprop <$> props HM.!? "Type")

-- A delta time iff this object is already alive
objectDelta :: Maybe ObjectState -> Double -> Maybe Double
objectDelta prev now = fmap (\o -> now - o.lastSeen) prev

typeOf :: ObjectState -> Text
typeOf os = fromMaybe "Unknown" os.tacType

updateTypeStats :: Maybe Double -> Text -> Map Text DeltaStats -> Map Text DeltaStats
updateTypeStats Nothing _ prev = prev -- Don't update if this is the 1st time seeing the object.
updateTypeStats (Just dt) objType prev = M.insertWith app objType first prev where
    app _new = appendDelta dt
    first = initStats dt

progress :: Maybe Integer -> Tacview.SourceProgress -> IO ()
progress mlen i = progress' mlen i 0

progress' :: Maybe Integer -> Tacview.SourceProgress  -> Int -> IO ()
progress' mlen i = fix $ \loop !n -> do
    i' <- readIORef i.lines
    if i' == 0
        then do
            hPutStr stderr "waiting for input on stdin..."
            let waitForInput = do
                    -- Polling is bad but I'll take it instead of busting out STM just yet.
                    threadDelay 100000 -- 20 FPS
                    i'' <- readIORef i.lines
                    if i'' == 0 then waitForInput else loop n
            waitForInput
        else do
            b <- readIORef i.bytes
            let cc = clearFromCursorToLineBeginningCode
                donePercent = case mlen of
                    Just len -> " (" <> show (round $ b % len * 100) <> "% done)"
                    Nothing -> ""
            T.hPutStr stderr . T.pack $ mconcat [
                cc,
                "\r",
                showChar (spinny n) " Read ",
                show i',
                " lines",
                donePercent
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
