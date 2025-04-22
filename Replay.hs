module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Channel
import Control.Concurrent.STM
import Control.Concurrent.TBCQueue
import Control.Monad
import Data.Fixed
import Data.Tacview (parseTime)
import Data.Tacview.Source qualified as Tacview
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options.Applicative
import System.Clock.Seconds
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
    (src, _, _) <- Tacview.source zipInput
    let pipe = pipeline (newTBCQueueIO 1024)
        delayer sink = pipe src (`delay` sink)
        writer = pipe delayer writeOut

    void writer

delay :: Channel c => c Text -> c Text -> IO ()
delay source sink = void $ stateConsumeChannel source Nothing $ \ !mdelta l -> do
    nd <- if T.isPrefixOf "#" l
        then do
            let t = parseTime l :: Double
            now <- getTime Monotonic
            case mdelta of
                -- If we don't have it yet,
                -- get the difference between now and the first timestamp.
                Nothing -> pure . Just $ realToFrac now - t
                -- Once we have it, apply that offset to the timestamp
                -- and sleep until then.
                Just d -> do
                    let sleepFor = (t + d) - realToFrac now :: Double
                    threadDelay . d2micro $ sleepFor
                    pure mdelta
        else pure mdelta
    atomically $ writeChannel' sink l
    pure nd

d2micro :: Double -> Int
d2micro d = fromIntegral m where
    MkFixed m = realToFrac d :: Micro

writeOut :: Channel c => c Text -> IO ()
writeOut source = consumeChannel source T.putStrLn
