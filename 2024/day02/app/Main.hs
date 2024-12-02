-- Vim: set sw=2 :
module Main where

import Data.Maybe (mapMaybe)

data Incline = Increasing | Decreasing | Invalid
  deriving (Show, Eq)

safeDescent :: [Int] -> [Incline]
safeDescent [_] = []
safeDescent (x1 : x2 : xs)
  | diff >= 1 && diff <= 3 = Increasing : safeDescent (x2 : xs)
  | diff >= -3 && diff <= -1 = Decreasing : safeDescent (x2 : xs)
  | otherwise = Invalid : safeDescent (x2 : xs)
  where
    diff = x2 - x1

allSame :: [Incline] -> Bool
allSame (x : xs)
  | (x : xs) == filter (== x) (x : xs) && x /= Invalid = True
  | otherwise = False

permutations :: [Int] -> [[Int]]
permutations xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

parseInput :: String -> [[Int]]
parseInput input = map (map read . words) (lines input)

main :: IO ()
main = do
  input <- getContents
  let levels = parseInput input
  print $ "p1: " ++ show (length (filter (allSame . safeDescent) levels))
  print $ "p2: " ++ show (length (filter (any (allSame . safeDescent) . permutations) levels))
