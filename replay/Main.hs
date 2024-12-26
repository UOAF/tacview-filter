module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Channel
import Control.Concurrent.STM
import Control.Monad
import Data.Fixed
import Data.Function (fix)
import Data.IORef
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
        help "Zipped ACMI to filter. Otherwise reads from stdin and writes to stdout",
        metavar "recording.zip.acmi"
        ]

main :: IO ()
main = do
    -- lol Windows
    hSetNewlineMode stdin noNewlineTranslation
    hSetNewlineMode stdout noNewlineTranslation
    hSetNewlineMode stderr noNewlineTranslation

    let parser = customExecParser (prefs showHelpOnError) parseInfo
        parseInfo = info (parseArgs <**> helper) $
            progDesc "Replays ACMI files at the rate they were recorded"
    parser >>= runFilter

runFilter :: Args -> IO ()
runFilter Args{..} = do
    linesRead <- newIORef 0
    let src = Tacview.source zipInput linesRead
        delayer sink = pipeline src (\source -> delay source sink Nothing)
        writer = pipeline delayer writeOut

    void writer

delay :: Channel Text -> Channel Text -> Maybe Double -> IO ()
delay source sink = fix $ \loop mdelta -> do
    let go l = do
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
            evalWriteChannel sink l
            loop nd
    atomically (readChannel source) >>= mapM_ go

d2micro :: Double -> Int
d2micro d = fromIntegral m where
    MkFixed m = realToFrac d :: Micro

writeOut :: Channel Text -> IO ()
writeOut source = atomically (readChannel source) >>= mapM_ go where
    go l = do
        T.putStrLn l
        writeOut source
