
import DataLoader

import Data.Word
import Text.Read
import Data.Function
import Data.List
import Data.Maybe

type Binary = [Int]

main = do
  dt <- map parseData <$> readInput
  print $ star1 dt
  print $ star2 dt

parseData :: String -> Binary
parseData s = mapMaybe (\v -> readMaybe [v] :: Maybe Int) s

star1 dt = decimal values * decimal (invert values)
  where
    n = length $ head dt
    values = map (mostCommon . fetch dt) [0..n-1]

fetch dt idx = map (\i -> i !! idx) dt

star2 dt = decimal positive * decimal negative
  where
    positive = head $ filterData True  dt 0
    negative = head $ filterData False dt 0
    n = length $ head dt



filterData :: Bool -> [Binary] -> Int -> [Binary]
filterData method [a] idx = [a]
filterData method remaining idx
 | length (filtered 1) == length (filtered 0) = filterData method (filtered $ fallback method) (idx + 1)
 | idx < length (head remaining)              = filterData method (filtered $ rule method) (idx + 1)
 | otherwise                                  = remaining
   where
     mc             = mostCommon
                    $ fetch remaining idx
     rule True      = mc
     rule False     = 1 - mc
     filtered tgt   = [k | k <- remaining, k !! idx == tgt]
     fallback True  = 1
     fallback False = 0


mostCommon :: [Int] -> Int
mostCommon v = fst $ maximumBy (compare `on` snd) ns
 where
   ns   = map (\x -> (x, length $ filter (==x) v)) $ nub v

invert :: [Int] -> [Int]
invert = map invert'
  where
    invert' 1 = 0
    invert' 0 = 1
    invert' x = x

decimal :: Binary -> Int
decimal d = sum
          $ zipWith power d [0..]
  where
    power 1 idx = 2 ^ (length d - idx -1)
    power _ _   = 0
