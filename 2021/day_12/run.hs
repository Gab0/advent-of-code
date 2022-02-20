import Puzzle.Input

import Data.Maybe
import Control.Monad
import Data.List
import Data.List.Split 
import Data.Char


data Location = Location
  { locationName :: String
  , locationDest :: [String]
  } 
  deriving Show


parseLocationMap :: [String] -> [Location]
parseLocationMap raw = foldl readLocLine [] raw
    where
        readLocLine :: [Location] -> String -> [Location]
        readLocLine locations s 
          | from `elem` all_ids = writeArray locations existent_idx updated_location
          | otherwise        = (Location from [to]):locations
            where
                [from, to] = splitOn "-" s

                all_ids = map locationName locations
                existent_idx = unsafeIndex from all_ids
                updated_location = addDestination (locations !! existent_idx) to


isMajor = isUpper . head

addDestination :: Location -> String -> Location
addDestination (Location n d) dest =  Location n (dest:d)

main :: IO ()
main = do
    location <- parseLocationMap <$> readRawInput
    print $ star1 location
    print $ star2 location

star1 location = length 
               $ navigate fpossible1 location ["start"]

star2 location = length
               $ navigate fpossible2 location ["start"]

navigate :: ([String] -> [String] -> [String]) 
         -> [Location] 
         -> [String] 
         -> [[String]]
navigate fpossible locations path
  | last path == "end" = [path]
  | otherwise          = 
      case possible of 
          [] -> []
          ps -> concatMap (navigate fpossible locations . (++) path . (:[])) ps
      where
          base_possible = getPossibleDestinations locations 
                        $ last path
          possible      = fpossible base_possible path

fpossible2 :: [String] -> [String] -> [String]
fpossible2 base_possible path
  | length (nub visited_minor) == length visited_minor = base_possible \\ ["start"]
  | otherwise                                          = fpossible1 base_possible path
    where
        visited_minor = filter (not . isMajor) path \\ mvp
        mvp = ["start", "end"]

fpossible1 :: [String] -> [String] -> [String]
fpossible1 base_possible path = base_possible \\ (filter (not . isMajor) path)

getPossibleDestinations :: [Location] -> String -> [String]
getPossibleDestinations locations position = forwards ++ backwards
    where
        forwards = case elemIndex position $ map locationName locations of
            Just k  -> locationDest $ locations !! k
            Nothing -> []
        backwards = getReverseMap locations position

getReverseMap :: [Location] -> String -> [String]
getReverseMap locations position = map locationName 
                                 $ filter (elem position . locationDest) locations

