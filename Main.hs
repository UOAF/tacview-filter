{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Main where

import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Text.Read qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word
import Options.Applicative

import GHC.Stack

-- No command line args at the moment...
-- Add flags to --ignore additional text?
-- Or we can just change this short program...
data Args = Args

parseArgs :: Parser Args
parseArgs = pure Args

-- --help text
main :: IO ()
main = parser >>= runFilter where
    parser = customExecParser (prefs showHelpOnError) parseInfo
    parseInfo = info (parseArgs <**> helper) $
        progDesc "Reads ACMI on stdin, filters crap, writes to stdout"

-- Boring I/O stuff: read stdin and split it into a list of lines.
inputLines :: IO [Text]
inputLines = do
    lazyLines <- TL.lines <$> TL.getContents
    pure $ TL.toStrict <$> lazyLines

type TacId = Word64

-- | Parse a hex value from the start of the given string
parseId :: HasCallStack => Text -> TacId
parseId t = case T.hexadecimal t of
    Left e -> error (T.unpack t <> ": " <> e)
    Right (v, _) -> v

-- | Parse a hex value from the start of the given string,
--   or return `Nothing` if it's not a hex value.
maybeId :: Text -> Maybe TacId
maybeId t = case T.hexadecimal t of
    Left _ -> Nothing
    Right (v, _) -> Just v

-- | For filtering purposes, each line is either:
data LineIds =
    -- | A property line: @id,prop1,prop2@
    --   This introduces objects if we haven't seen them before.
    PropLine TacId |
    -- | A removal line: @-id@
    RemLine TacId |
    -- | An event line: @0,Event=...|id1|id2|...@
    --   This can have multiple IDs!
    EventLine (HashSet TacId)
    deriving (Show)

-- | Parse the `LineIds` of the line.
--   (or `Nothing` if it's not a line that affects filtering)
idsOf :: Text -> Maybe LineIds
idsOf l
    | T.isPrefixOf "0,Event=" l = let
        toks = tail $ T.splitOn "|" l
        ids = mapMaybe maybeId toks
        in Just . EventLine $ HS.fromList ids
    | T.isPrefixOf "-" l = Just . RemLine $ parseId (T.tail l)
    | otherwise = case maybeId l of
        Just i -> Just $ PropLine i
        Nothing -> Nothing

runFilter :: Args -> IO ()
runFilter _ = do
    ls <- inputLines -- Read input
    let filtered = filteredLines startState ls -- Filter it...
    mapM_ T.putStrLn filtered -- And print it back out, line at a time.

-- | Things we track as we iterate through the lines:
data FilterState = FilterState {
    -- | What are objects who should we totally ignore
    --   if their properties contains one of these strings?
    fsIgnore :: Vector Text,
    -- | What are objects whose events we should ignore
    --   if their properties contains one of these strings?
    fsUneventful :: Vector Text,
    -- | What objects (by ID) are we currently ignoring?
    fsIgnored :: HashSet TacId,
    -- | What objects' (by ID) events are we currently ignoring?
    fsEventsIgnored :: HashSet TacId
}

startState :: FilterState
startState = FilterState{..} where
    -- Tacview actually records all buildings and static objects,
    -- but I never figured out how to give them a size or shape to show up.
    -- I was hoping it would be helpful to at least know when you hit something,
    -- but...
    fsIgnore = V.fromList [ "Type=Ground+Static" ]
    -- It's really dumb to get hit/timeout messages for ATGM rounds,
    -- which the ground AI shoots like they're machine gun ammo.
    fsUneventful = V.fromList [
        "Name=BGM-71 TOW",
        "Name=M72 LAW",
        "Name=M47 Dragon ATGM",
        "Name=AT-4" ]
    fsIgnored = HS.empty
    fsEventsIgnored = HS.empty

-- | Given a line and its object ID, update the list of things we're ignoring
--   and the list of things whose events we're ignoring.
updateIgnores :: TacId -> Text -> FilterState -> FilterState
updateIgnores i l fs = fs { fsIgnored = newIg, fsEventsIgnored = newEvIg } where
    (newIg, newEvIg) = go
    go
      -- If the line matches against any of our strings to ignore,
      -- insert the ID into the ignore set
      | any (`T.isInfixOf` l) fs.fsIgnore = (HS.insert i fs.fsIgnored, fs.fsEventsIgnored)
      -- If the line matches against any of our strings of events to ignore,
      -- insert the ID into the events ignore set
      | any (`T.isInfixOf` l) fs.fsUneventful = (fs.fsIgnored, HS.insert i fs.fsEventsIgnored)
      -- Otherwise everything is unchanged.
      | otherwise = (fs.fsIgnored, fs.fsEventsIgnored)

-- | Remove the given object ID from the list of things we're ignoring
--   and the list of things whose events we're ignoring.
removeId :: TacId -> FilterState -> FilterState
removeId i fs = fs { fsIgnored = newIg, fsEventsIgnored = newEvIg } where
    newIg = HS.delete i fs.fsIgnored
    newEvIg = HS.delete i fs.fsEventsIgnored

-- We can ignore an event if all of its IDs are in the
-- "ignored" or "events ignored" sets
ignoreableEvent :: HashSet TacId -> FilterState -> Bool
ignoreableEvent es fs = all (`HS.member` toIgnore) es where
    toIgnore = HS.union fs.fsIgnored fs.fsEventsIgnored

filteredLines :: FilterState -> [Text] -> [Text]
filteredLines _ [] = [] -- Base case: an empty list has nothing to filter.

-- Our filtered lines are the current line (or nothing if it's filtered out)
-- plus the rest of the list, filtered. Recursion!
filteredLines fs (l:rest) = filtered ++ filteredLines nextState rest where
    (filtered, nextState) = go $ idsOf l
    -- Property lines will update our ignore lists,
    -- then get filtered on the _updated_ version of those, fs'
    go (Just (PropLine p)) = (l', fs') where
        fs' = updateIgnores p l fs
        l' = [l | not (HS.member p fs'.fsIgnored)]
    -- Skip a removal line if we're ignoring the object.
    -- The next state is the current one with the ID removed.
    go (Just (RemLine r)) = (l', fs') where
        l' = [l | not (HS.member r fs.fsIgnored)]
        fs' = removeId r fs
    -- Skip an event if it's ignoreable.
    go (Just (EventLine es)) = (l', fs) where
        l' = [l | not (ignoreableEvent es fs)]
    -- Pass the current line through, no change to state.
    go Nothing = ([l], fs)
