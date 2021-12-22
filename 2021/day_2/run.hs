 
import DataLoader

import Data.List.Split
import Data.Maybe
import Text.Read

main = do
  dt <- mapMaybe parseData <$> readRawInput

  print $ star1 dt
  print $ star2 dt

parseData :: String -> Maybe (Int, Int)
parseData "" = Nothing
parseData x = case multiplier of
  Nothing -> Nothing
  Just m  -> Just $ mulVec2 (p1, p2) m
  where
    pos "down"    = (0, 1)
    pos "up"      = (0, -1)
    pos "forward" = (1, 0)
    k     = take 2 $ splitOn " " x
    svec  = head k
    n = last k
    multiplier    = readMaybe n :: Maybe Int
    (p1, p2)      = pos svec


star1 :: [(Int, Int)] -> Int
star1 v = d * depth
  where
    (d, depth) = navigate v (0, 0)

star2 :: [(Int, Int)] -> Int
star2 v = d * depth
  where
    (d, depth, aim) = navigateAim v (0, 0, 0)

navigateAim :: [(Int, Int)] -> (Int, Int, Int) -> (Int, Int, Int)
navigateAim [] position     = position
navigateAim (x:xs) position = navigateAim xs (ndist, ndepth, naim)
  where
    (f, aim_adjust)    = x
    (dist, depth, aim) = position
    naim               = aim + aim_adjust

    ndist              = dist + f
    ndepth             = depth + f * aim



navigate :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
navigate [] position     = position
navigate (x:xs) position = navigate xs new_position
  where
    new_position  = sumVec2 position x
