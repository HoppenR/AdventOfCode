-- Vim: set sw=2 :
module Main where

data Incline = Increasing | Decreasing | Invalid
  deriving (Show, Eq)

getIncline :: [Int] -> [Incline]
getIncline [_] = []
getIncline (x1 : x2 : tl)
  | diff >= 1 && diff <= 3 = Increasing : getIncline (x2 : tl)
  | diff >= -3 && diff <= -1 = Decreasing : getIncline (x2 : tl)
  | otherwise = [Invalid]
  where
    diff = x2 - x1

allSame :: [Incline] -> Bool
allSame (Invalid : _) = False
allSame (hd : tl) = all (== hd) tl

permutations :: [Int] -> [[Int]]
permutations xs = [take i xs ++ drop (succ i) xs | i <- [0 .. length xs - 1]]

parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

main :: IO ()
main = do
  input <- getContents
  let levels = parseInput input
  print $ "p1: " ++ show (length (filter (allSame . getIncline) levels))
  print $ "p2: " ++ show (length (filter (any (allSame . getIncline) . permutations) levels))
