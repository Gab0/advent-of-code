{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Puzzle.Input
import qualified Data.Text as T
import qualified Data.Map as Map

type Digit = String
data Display = Display [Digit] [Digit]
    deriving Show

data Wire = Wire Char Char
    deriving (Show, Eq)

type Wireset = [Wire]

allChars = "abcdefg"

numberMap = [ "abcefg"
            , "cf"
            , "acdeg"
            ,"acdfg"
            , "bcdf"
            , "abdfg"
            , "abdefg"
            , "acf"
            , "abcdefg"
            , "abcdfg"
            ]

uniqueN :: Map.Map Int Int
uniqueN = Map.fromList [(2, 1), (3, 7), (4, 4), (7, 8)]

-- | This is the hardest challenge so far,
-- mostly because there seems to be many ways of completing this.
-- Most possible methods are misleading and/or veery complicated to implement.
-- Once the correct 'mindset' was found, it was a breeze.
-- 
-- STAR 2 was solved by generating all possible wire patterns,
-- then checking if each wire pattern can translate all the numbers 
-- in the display and have them match any base number pattern.
-- This test is sufficient to rule out the single correct wire pattern.
main :: IO ()
main = do
    codes <- map parseDisplay <$> readRawInput
    print $ star1 codes
    print $ star2 codes

star1 = sum . concatMap checkEasyNumber 

star2 = sum . map decipherDisplay

checkEasyNumber :: Display -> [Int]
checkEasyNumber (Display _ k) = map (fromEnum . checkEasy) k
    where
        checkEasy n = length (nub n) `elem` (Map.keys uniqueN)
        
parseDisplay :: String -> Display
parseDisplay inp = Display (map T.unpack a) (map T.unpack b)
    where
        [a, b] = map (T.splitOn " ") 
               $ T.splitOn " | " 
               $ T.pack inp

base10 :: [Int] -> Int 
base10 vec = foldl cnv 0 $ zip vec $ reverse [0..length vec-1]
    where
        cnv b (v, idx) = b + v  * 10 ^ idx

decipherDisplay :: Display -> Int 
decipherDisplay (Display t k) = base10 $ map (renderNumber wireMap) k
    where 
        wireMap  = head 
                 $ filter (testWireset (t ++ k )) generateAllWiresets

generateAllWiresets :: [Wireset]
generateAllWiresets = map makeWireset 
                    $ permutations allChars
    where
        makeWireset = map (uncurry Wire) . zip allChars

testWireset :: [String] -> Wireset -> Bool
testWireset numbers wireset = all id
                            $ map (checkWireset wireset) numbers
    where
        checkWireset :: Wireset -> String -> Bool
        checkWireset wireset number = convertNumber wireset number `elem` numberMap

isEasyNumber :: String -> Bool
isEasyNumber s = length s `elem` Map.keys uniqueN

renderNumber :: [Wire] -> String -> Int
renderNumber wireset n = unsafeIndex (convertNumber wireset n) numberMap

convertNumber :: [Wire] -> String -> String
convertNumber wireset s = sort converted
    where
        (_, converted)  = foldl convertWire (s, "") wireset

convertWire :: (String, String) -> Wire -> (String, String)
convertWire (inp, out) (Wire f t)
    | f `elem` inp = (inp, t:out)
    | otherwise    = (inp, out)