import Puzzle.Input

import Data.Maybe
import Data.List

data ParseResult = ParseResult (Maybe Char) String

main :: IO ()
main = do
    codelines <- readRawInput
    print $ star1 codelines
    print $ star2 codelines

star1 = sum 
      . map getScore 
      . mapMaybe (\(ParseResult r b) -> r) 
      . map getOffendingChar
star2 codelines = head $ drop drop_n scores
    where 
        scores = sort 
               $ mapMaybe evaluateBuffer 
               $ map getOffendingChar codelines
        drop_n = floor
               $ (fromIntegral $ length scores) / 2
    

evaluateBuffer :: ParseResult -> Maybe Int
evaluateBuffer (ParseResult Nothing buffer) = Just $ bufferScore buffer
evaluateBuffer _                            = Nothing


bufferScore :: String -> Int
bufferScore s = foldl calculateFold 0 s
    where 
        charScore c = 1 + unsafeIndex c closers

        calculateFold :: Int -> Char -> Int
        calculateFold prev c = prev * 5 + charScore c

getScore k = scores !! (unsafeIndex k $ closers)
    where
        scores = [3, 57, 1197, 25137]

getOffendingChar :: String -> ParseResult
getOffendingChar = readCode ""
    where
        readCode :: String -> String -> ParseResult
        readCode buffer "" = ParseResult Nothing buffer
        readCode buffer (x:xs)
         | isOpen x  = readCode (findDelimPair x:buffer) xs
         | x == head buffer    = readCode (tail buffer) xs
         | otherwise = ParseResult (Just x) buffer

isOpen :: Char -> Bool
isOpen c = c `elem` map head delimiters

findDelimPair :: Char -> Char
findDelimPair c = closers !! unsafeIndex c openers

delimiters = 
    [ "()"
    , "[]"
    , "{}"
    , "<>"
    ]

openers = map head delimiters

closers = map last delimiters