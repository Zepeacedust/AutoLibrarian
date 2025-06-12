module Lookup where

import Definitions
import qualified Data.Text as T
import qualified Data.List as L
import Text.Regex.Posix
import qualified Data.Array as Arr
import Data.Array ((!))
import Data.Ord

spellRegex :: String
spellRegex = "\\[\\[([^]]*)\\]\\]"

getMentionedSpells :: String -> [String]
getMentionedSpells input =
    let
         strLs = (input =~ spellRegex) :: [[String]]
    in  map (head . tail) strLs

diff :: T.Text -> T.Text -> Int
diff a b = memo 0 0 where
    helperFunc :: Int -> Int -> Int
    helperFunc x y
        | T.index a x == T.index b y = modded
        | otherwise = minimum  [
            1+subbed,
            1+added,
            1+modded
            ]
        where
            modded = memo (x+1) (y+1)
            added  = memo (x  ) (y+1)
            subbed = memo (x+1) (y  )
    memo x y
      | x >= lA = lB - y
      | y >= lB = lA - x
      | otherwise     = arr ! (x, y)
    arr = Arr.listArray bound [helperFunc x y | (x,y) <- Arr.range bound]
    bound = ((0,0),(lA, lB))
    lA = T.length a
    lB = T.length b


lookupSpell :: [Spell] -> T.Text -> Spell
lookupSpell grimoire target = L.minimumBy (comparing (diff (T.toUpper target) . name)) grimoire
