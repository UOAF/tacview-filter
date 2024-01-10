module Utils where

import Data.Ratio
import Text.Printf

-- | Express n/d as both that ratio and a percentage
percentage :: Int -> Int -> String
percentage n d = let p = realToFrac $ n % d * 100 :: Double
    in printf "%d/%d (%.2f%%)" n d p

