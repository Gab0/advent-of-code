import DataLoader

import           Data.List.Split
import qualified Data.Text as T
import           Data.List

type Point = (Int, Int)

data Line = Line Point Point
  deriving (Show, Eq)

main = do
  dt <- map parseLine <$> (readRawInput :: IO [String])
  print $ star1 dt
  print $ star2 dt

star1 dt = countOverlaps
         $ concatMap getLinePoints
         $ filter (isHV) dt

star2 dt = countOverlaps
         $ concatMap getLinePoints dt

countOverlaps :: [Point] -> Int
countOverlaps  = length
               . filter ((<) 1 . length)
               . group
               . sort
               . map convert
  where
    convert :: Point -> Int
    convert (x, y) = (x * 1000) + y

parseLine :: String -> Line
parseLine t = Line p1 p2
  where
    [p1, p2]    = map readPoint
                $ splitOn " -> " t
    readPoint p = (x, y)
      where
        [x, y] = map (read :: String -> Int)
               $ splitOn "," p

isHV :: Line -> Bool
isHV (Line (ax, ay) (bx, by)) = ax == bx
                             || ay == by

getLinePoints :: Line -> [Point]
getLinePoints (Line (x1, y1) (x2, y2))
  | x1 == x2  = map (\y -> (x1, y)) $ range y1 y2
  | y1 == y2  = map (\x -> (x, y1)) $ range x1 x2
  | otherwise = zip (range x1 x2) (range y1 y2)

range :: Int -> Int -> [Int]
range a b
  | a < b     = [a..b]
  | otherwise = reverse [b..a]
