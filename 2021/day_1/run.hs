

import DataLoader

main = do
  v <- loadData :: IO [Int]
  print $ star1 v
  print $ star2 v

star1 :: [Int] -> Int
star1 v = length $ filter (==True) $ zipWith (<) v (tail v)


star2 v = star1 $ window3 v []

window3 :: [Int] -> [Int] -> [Int]
window3 rest windows
 | length rest >= 3 = window3 (tail rest) (windows ++ [sum $ take 3 rest])
 | otherwise        = windows
