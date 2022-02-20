module Puzzle.Input where

import System.IO
import System.Environment

import Data.List.Split
import Data.List

import Control.Monad (join)
import Control.Arrow ((***))

parseFile :: String -> IO [String]
parseFile fpath = do
  handle   <- openFile fpath ReadMode
  contents <- hGetContents handle

  return $ lines contents

parseIntGrid :: [String] -> [[Int]]
parseIntGrid = map (map readValue)
    where
        readValue :: Char -> Int
        readValue = read . (:[])

dataArray :: (Read a) => [String] -> [a]
dataArray = map read

readRawInput :: IO [String]
readRawInput = head <$> getArgs >>= parseFile

readParsedInput :: (Read a) => IO [a]
readParsedInput = dataArray <$> readRawInput

-- | Read input when it is a single line of comma separate values.
readCSVLine :: IO [Int]
readCSVLine = dataArray . splitOn "," . head <$> readRawInput

sumVec2 (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

mulVec2 (a1, a2) n = (a1 * n, a2 * n)

unsafeIndex :: (Eq a) => a -> [a] -> Int
unsafeIndex v xs = case elemIndex v xs of Just k -> k

writeArray :: [a] -> Int -> a -> [a]
writeArray m idx v = take idx m ++ [v] ++ drop (idx + 1) m