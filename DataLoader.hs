module DataLoader where

import System.IO
import System.Environment

import Control.Monad (join)
import Control.Arrow ((***))

parseFile :: String -> IO [String]
parseFile fpath = do
  handle   <- openFile fpath ReadMode
  contents <- hGetContents handle

  return $ lines contents

dataArray :: (Read a) => [String] -> [a]
dataArray = map read


readRawInput :: IO [String]
readRawInput = head <$> getArgs >>= parseFile

readParsedInput :: (Read a) => IO [a]
readParsedInput = dataArray <$> readRawInput


sumVec2 (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

mulVec2 (a1, a2) n = (a1 * n, a2 * n)
