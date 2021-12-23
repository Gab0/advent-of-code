
import DataLoader
import Data.List.Split

type State = [Int]

main = do
  state <- parse . head
       <$> readRawInput
  print $  star1 state
  print $  star2 state

parse :: String -> [Int]
parse = map (read :: String -> Int) . splitOn ","

star1 state = sum $ simulateSmart (toSmartState state) 80

star2 state = sum $ simulateSmart (toSmartState state) 256

toSmartState state = [length $ filter (==c) state | c <- [0..8]]

-- | This will take too long.
simulate :: State -> Int -> State
simulate state 0 = state
simulate state t = simulate new_state $ t - 1
  where
    new_state = map modInd state ++ (replicate n_new 8)
    n_new     = length $ filter (==0) state
    modInd 0  = 6
    modInd n  = n - 1

simulateSmart :: State -> Int -> State
simulateSmart xs 0 = xs
simulateSmart xs n = simulateSmart (tail new_state ++ [head new_state]) $ n - 1
  where
    new_state = take 7 xs ++ [head xs + xs !! 7, xs !! 8]
