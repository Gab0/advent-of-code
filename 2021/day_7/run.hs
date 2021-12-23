
import DataLoader

main = do
  dt <- readCSVLine :: IO [Int]
  print $ star1 dt
  print $ star2 dt

star1 = minimum . calculateScores id

star2 = minimum . calculateScores composed

calculateScores fn crabpos = map calculateForReference [0..maximum crabpos]
  where
    calculateForReference ref = sum $ map (fn . abs . (-) ref) crabpos

composed :: Int -> Int
composed 0 = 0
composed 1 = 1
composed n = n  + composed (n-1)
