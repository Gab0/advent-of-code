
import Puzzle.Input
import Data.List
import Data.List.Split

data Grid = Grid [Int] Int

type Position = (Int, Int)

class Grid2D g where
    readV        :: g -> (Int, Int) -> Int
    writeV       :: g -> (Int, Int) -> Int -> Grid
    getPos       :: g -> (Int, Int) -> Int
    getCoord     :: g ->  Int       -> Position
    isCoordValid :: g -> (Int, Int) -> Bool

instance Grid2D Grid where
    readV  grid@(Grid m w)         = (!!) m . getPos grid
    writeV grid@(Grid m w) coord v = Grid new_m w
        where 
            idx   = getPos grid coord 
            new_m = writeArray m idx v

    getPos (Grid m w) (x, y) = x + y * w
    getCoord (Grid m w) idx = (idx `mod` w, floor $ (fromIntegral idx) / (fromIntegral w))
    isCoordValid grid@(Grid m w) (x, y) =  not 
                                        $ x < 0 
                                       || y < 0 
                                       || x >= w 
                                       || y >= floor ((fromIntegral $ length m) / (fromIntegral w))
instance Show Grid where
    show (Grid m w) = intercalate "\n" 
                    $ map (intercalate "" . map show) 
                    $ chunksOf w m 

initGrid :: [[Int]] -> Grid
initGrid m = Grid (concat m) (length m)

main :: IO ()
main = do 
    octopus <- initGrid . parseIntGrid <$> readRawInput
    print $ star1 octopus
    print $ star2 octopus

star1 octopus = length $ filter id $ concat f
    where (st, f) = simulate octopus 100 []

star2 octopus = 1 + unsafeIndex (replicate (length $ head f) True) f
    where (st, f) = simulate octopus 1000 []



simulate :: Grid -> Int -> [[Bool]] -> (Grid, [[Bool]])
simulate grid 0 nflashes              = (grid, nflashes)
simulate (Grid m w) epoch2go flashes = simulate final_grid (epoch2go -1) (flashes ++ [new_flashed])
    where
        base_grid = Grid (map ((+) 1) m) w
        (final_grid, new_flashed) = resetFlashed 
                                  $ simulateFlashes initial_state
        initial_state = (base_grid, replicate (length m) False)

simulateFlashes :: (Grid, [Bool]) -> (Grid, [Bool])
simulateFlashes (grid@(Grid m w), flashed)
    | null flash_todo = (grid, flashed)
    | otherwise       = simulateFlashes 
                      $ foldl executeFlash (grid, flashed) flash_todo
    where
        flash_todo = filter checkFlash'
                   $ zip3 m flashed idxs
        idxs       = [0..length m-1]

checkFlash' :: (Int, Bool, Int) -> Bool
checkFlash' (v, flashed, idx) = v > 9 && not flashed

resetFlashed :: (Grid, [Bool]) -> (Grid, [Bool])
resetFlashed (Grid m w, flashed) = (Grid new_m w, flashed)
    where
        new_m = map reset' 
              $ zip m flashed
        reset' (_, True)  = 0
        reset' (v, False) = v

executeFlash :: (Grid, [Bool]) -> (Int, Bool, Int) -> (Grid, [Bool])
executeFlash (grid@(Grid m w), flashed) (_, _, idx) = (new_grid, new_flashed)
    where
        neighborhood = filter (isCoordValid grid) 
                     $ getNeighborhood 
                     $ getCoord grid idx
        new_flashed  = writeArray flashed idx True
        new_grid     = foldl increaseV grid neighborhood

increaseV :: Grid -> Position -> Grid
increaseV grid pos = writeV grid pos (v + 1)
    where v = readV grid pos

-- | There must be a better way to generate the neighborhood coordinates.
getNeighborhood :: Position -> [Position]
getNeighborhood (x, y) = [ (x-1, y-1), (x, y-1), (x+1, y-1)
                         , (x-1, y  ),           (x+1, y  )
                         , (x-1, y+1), (x,y+1 ), (x+1, y+1)
                         ]
