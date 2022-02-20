
{-# LANGUAGE OverloadedStrings #-}
import Puzzle.Input

import qualified Data.Text as T
import           Data.List
import           Data.Maybe
import           Data.List.Split

type Board = [[Int]]
type Calls = [Int]


main = do
  dt <- readRawInput :: IO [String]
  let calls  = map parseInt
             $ T.splitOn ","
             $ T.pack
             $ head dt
  let boards = boardParser (drop 1 dt) []
  print $ star1 calls boards
  print $ star2 calls boards
  where
    parseInt :: T.Text -> Int
    parseInt = read . T.unpack

star1 calls boards = executeCalls boards calls []

star2 calls boards = checkBoardScore (reverse $ take (timePoint + 1) calls) (boards !! tgtBoardIndex)
  where
    timePoint    = maximum indexes
    finalCall    = track !! tgtBoardIndex
    indexes      = [safeIndex t calls | t <- track]
    track        = executeCallsTrack boards [0 | x <- boards] calls []
    tgtBoardIndex = safeIndex (maximum indexes) track

safeIndex :: Int -> [Int] -> Int
safeIndex v xs = case elemIndex v xs of Just k -> k

executeCalls :: [Board] -> Calls -> Calls -> Int
executeCalls boards remaining done
  | all (==0) score = executeCalls boards (tail remaining) (head remaining:done)
  | otherwise       = maximum score
  where
    score = map (checkBoardScore done) boards

executeCallsTrack :: [Board] -> [Int] -> Calls -> Calls -> [Int]
executeCallsTrack _ track [] _       = track
executeCallsTrack boards track remaining done
  | null (filter (==0) score)        = track
  | otherwise                        = executeCallsTrack boards new_track (tail remaining) (head remaining:done)
  where
    score       = map (checkBoardScore done) boards
    prevScore   = map (checkBoardScore (tail done)) boards
    targetBoard = safeIndex 0 prevScore
    new_track   = map updateTrack [0..(length track - 1)]
    updateTrack idx
      | track !! idx > 0                         = track !! idx
      | checkBoardScore done (boards !! idx) > 0 = head done
      | otherwise                                = 0

boardParser :: [String] -> [Board] -> [Board]
boardParser []      boards = boards
boardParser content boards = boardParser (drop 6 content) (board:boards)
  where
    board  = map (parseBoardLine . T.replace "  " " " . T.pack) usable
    parseBoardLine :: T.Text -> [Int]
    parseBoardLine = map (read . T.unpack . T.strip)
                   . filter (/= "")
                   . T.splitOn " "
    usable = take 5 $ drop 1 content

checkBoardScore :: Calls -> Board -> Int
checkBoardScore calls board
  | gotRow || gotCol = head calls * sum non_called
  | otherwise        = 0
  where
    gotRow = checkT board
    gotCol = checkT $ transpose board
    checkT :: Board -> Bool
    checkT = any (==True) . map (all (==True) . map (\v -> v `elem` calls))
    non_called = [x | x <- concat board, not $ x `elem` calls]
