import Puzzle.Input
import Data.Ord
import Data.List
import Data.Maybe

type Map = [[Int]]
type Position = (Int, Int)


main :: IO ()
main = do
    height_map <- parseIntGrid <$> readRawInput
    print $ star1 height_map
    print $ star2 height_map

star1 :: Map -> Int
star1 height_map = sum 
                 $ map (calculateLowPointScore height_map) 
                 $ getLowPoints height_map

star2 :: Map -> Int 
star2 height_map = foldl1 (*) 
                 $ map length 
                 $ take 3 
                 $ reverse 
                 $ sortBy (comparing length) 
                 $ map (growBasin height_map . (:[])) 
                 $ getLowPoints height_map


calculateLowPointScore :: Map -> Position -> Int
calculateLowPointScore height_map position =
     case getV height_map position of 
        Just v  -> v + 1
        Nothing -> 0

getLowPoints :: Map -> [Position]
getLowPoints height_map = mapMaybe isLowPoint coordinates
    where
        sizeY = length height_map
        sizeX = length $ head height_map
        coordinates = [(x, y) | x <- [0..sizeX-1] , y <- [0..sizeY-1]]
        isLowPoint (x, y) = 
            case all (> v) $ mapMaybe (getV height_map) $ getNeighborhood (x, y) of
                True  -> Just (x, y)
                False -> Nothing
            where
                v = case getV height_map (x, y) of Just t -> t

getV :: Map -> Position -> Maybe Int
getV height_map (x, y) 
    | x < 0 || x >= sizeX || y < 0 || y >= sizeY = Nothing
    | otherwise                                  = Just $ height_map !! x !! y
    where
        sizeX = length $ head height_map
        sizeY = length height_map

getNeighborhood :: Position -> [Position]                  
getNeighborhood (x, y) = [ (x, y-1)
                         , (x-1, y)
                         , (x+1, y)
                         , (x, y+1)  
                         ]

growBasin :: Map -> [Position] -> [Position]
growBasin height_map current 
  | length current == length new_coords = current
  | otherwise                           = growBasin height_map new_coords
    where
        new_coords = filter (canEnterBasin) $ nub $ current ++ concatMap getNeighborhood current
        canEnterBasin (x, y) =
            case  getV height_map (x, y) of
                Just 9  -> False
                Nothing -> False
                Just x  -> True

