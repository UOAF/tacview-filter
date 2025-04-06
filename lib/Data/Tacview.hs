{-# LANGUAGE DeriveGeneric #-}

module Data.Tacview where

import Control.DeepSeq
import Data.Char (isDigit)
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
import Text.Printf

import GHC.Generics
import GHC.Stack

zipExt :: String
zipExt = ".zip.acmi"

txtExt :: String
txtExt = ".txt.acmi"

-- | Objects in TacView are identified by 64-bit hex values.
type TacId = Word64

-- | Pull the (#) off the front of the line and parse the rest as a double.
parseTime :: HasCallStack => Text -> Double
parseTime t = case T.rational (T.tail t) of
    Left e -> error (T.unpack t <> ": " <> e)
    Right (v, _) -> v

-- | Parse a hex value from the start of the given string.
parseId :: HasCallStack => Text -> TacId
parseId t = case T.hexadecimal t of
    Left e -> error (T.unpack t <> ": " <> e)
    Right (v, _) -> v

-- | Parse a hex value from the start of the given string,
--   or return `Nothing` if it's not a hex value.
maybeId :: Text -> Maybe TacId
maybeId t = case T.hexadecimal t of
    Left _ -> Nothing
    -- "File" parses as (0xf, "ile"). Let's not do that
    Right (v, rest) -> if rest == "" || T.isPrefixOf "," rest then Just v else Nothing

-- | For filtering purposes, each line is either:
data ParsedLine =
    TimeLine Double |
    -- | A property line: @id,prop1,prop2@
    --   This introduces objects if we haven't seen them before.
    PropLine TacId Properties |
    -- | A removal line: @-id@
    RemLine TacId |
    -- | An event line: @0,Event=...|id1|id2|...@
    --   This can have multiple IDs!
    EventLine (HashSet TacId) Text |
    -- | Global config or something else we don't care to parse
    OtherLine Text
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

showLine :: ParsedLine -> Text
showLine (TimeLine t) = "#" <> (shaveZeroes . T.pack $ printf "%f" t)
showLine (PropLine i p) = (T.pack . printf "%x," $ i) <> showProperties p
showLine (RemLine i) = T.pack $ printf "-%x" i
showLine (EventLine _is l) = l
showLine (OtherLine l) = l

-- | Parse the `LineIds` of the line.
--   (or `Nothing` if it's not a line that affects filtering)
parseLine :: Text -> ParsedLine
parseLine l
    | T.isPrefixOf "#" l = TimeLine $ parseTime l
    | T.isPrefixOf "0,Event=" l = let
        toks = tail $ T.splitOn "|" l
        ids = mapMaybe maybeId toks
        in EventLine (HS.fromList ids) l
    | T.isPrefixOf "-" l = RemLine $ parseId (T.tail l)
    | T.isPrefixOf "0," l = OtherLine l -- Don't try to parse global config.
    | otherwise = case maybeId l of
        Just i -> PropLine i (lineProperties l)
        Nothing -> OtherLine l

-- | Positions are a special case, where each coordinate can be delta-encoded.
data Property = Property Text | Position (Vector Text)
    deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

showProperty :: Property -> Text
showProperty (Property t) = shaveZeroes t
showProperty (Position v) = T.intercalate "|" $ shaveZeroes <$> V.toList v

shaveZeroes :: Text -> Text
shaveZeroes t = if T.all (\c -> isDigit c || c == '.') t
    then let
        go [whole, fractional] = let
            trimmedFrac = T.dropWhileEnd (== '0') fractional
            in if T.null trimmedFrac
                then whole
                else whole <> "." <> trimmedFrac
        go _ = t
        in go $ T.splitOn "." t
    else t

-- | Each property is <name>=<value>
parseProperty :: Text -> (Text, Property)
parseProperty pl = (k, p) where
    (k, vWithEq) = T.breakOn "=" pl
    v = T.tail vWithEq
    p = if k == "T" -- Positions have the key "T" and get special treatment
        then Position . V.fromList . T.splitOn "|" $ v
        else Property v

type Properties = HashMap Text Property

-- | The opposite of parse
showProperties :: Properties -> Text
showProperties ps = -- Finagling - get T= first
    T.intercalate "," $ maybeT ++ rest where
        maybeT = case ps HM.!? "T" of
            Just t -> [eqPair ("T", t)]
            Nothing -> []
        rest = eqPair <$> HM.toList (HM.delete "T" ps)
        eqPair (k, v) = k <> "=" <> showProperty v

-- | Properties are comma-separated, with the first one being the `TacId`
-- (which we've already parsed)
lineProperties :: Text -> Properties
lineProperties t = HM.fromList $ fmap parseProperty (tail . T.splitOn "," $ t)

-- | Update a pervious set of object properties with the new set
-- (Updates are delta-encoded where unmentioned properties retain their previous value.)
updateProperties :: Properties -> Properties -> Properties
updateProperties = HM.unionWith updateProperty

-- | Given the previous version of a property and an update, return its new value.
-- For everything but positions, this is just the new value.
-- For positions, individual coordinates in the position can be delta-encoded.
updateProperty :: Property -> Property -> Property
updateProperty (Property _old) new@(Property _) = new
updateProperty (Position old) (Position new) = if V.length old == V.length new
    then Position $ V.zipWith maybeCoord old new
    else Position new
updateProperty _ _ = error "Mixing properties and positions"

-- | For coordinates: Replace blanks with not blanks.
-- We're trying to accumulate data here.
maybeCoord :: Text -> Text -> Text
maybeCoord old new
    | old == "" = new
    | new == "" = old
    | otherwise = new

-- | Calculate the delta between an old set of object properties and the new set.
-- Used to determine what we should write out.
deltaProperties :: Properties -> Properties -> Properties
deltaProperties old new = HM.differenceWith (flip deltaProperty) new old

deltaProperty :: Property -> Property -> Maybe Property
deltaProperty (Property old) (Property new) = if old == new then Nothing else Just (Property new)
deltaProperty (Position old) (Position new) = if V.length old == V.length new
    then let
        go coords -- If none of the coords have changed, skip T=||| crap.
            | V.all (== "") coords = Nothing
            | otherwise = Just (Position coords)
        in go $ V.zipWith deltaCoord old new
    else Just (Position new) -- If the coordinate type change, give us the new one.
deltaProperty _ _ = error "Mixing properties and positions"

deltaCoord :: Text -> Text -> Text
deltaCoord old new = if old == new then "" else new

-- | Delta-encode an object, generating a line with only properties that changed.
-- Returns Just the line, or Nothing if there's no changes.
deltaEncode :: Properties -> Properties -> Maybe Properties
deltaEncode old new = let
    deltaProps = deltaProperties old new
    in if HM.null deltaProps
        -- Don't write if the delta-encoded version is empty (nothing changed).
        then Nothing
        else Just deltaProps
