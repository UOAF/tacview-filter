{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Data.Tacview where

import Control.DeepSeq
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Data.Word
import Data.Vector (Vector)
import Data.Vector qualified as V

import GHC.Generics
import GHC.Stack

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
    deriving (Show, Generic)

instance NFData LineIds

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

data Property = Property Text | Position (Vector Text)

parseProperty :: Text -> (Text, Property)
parseProperty pl = (k, p) where
    (k, vWithEq) = T.breakOn "=" pl
    v = T.tail vWithEq
    p = if k == "T"
        then Position . V.fromList . T.splitOn "|" $ v
        else Property v

type ParsedProperties = HashMap Text Property

lineProperties :: Text -> ParsedProperties
lineProperties t = HM.fromList $ fmap parseProperty (tail . T.splitOn "," $ t)

updateProperty :: Property -> Property -> Property
updateProperty (Property _old) new@(Property _) = new
updateProperty (Position old) (Position new) = error "TODO: merge positions"
updateProperty _ _ = error "Mixing properties and positions"
