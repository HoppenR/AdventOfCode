-- Vim: set sw=2 :
module Main where

import Data.Bifunctor (bimap)

validateRule :: [Int] -> (Int, Int) -> Bool
validateRule [] _ = True
validateRule (x : xs) rules@(rule1, rule2)
  | x == rule1 = True
  | x == rule2 = rule1 `notElem` xs
  | otherwise = validateRule xs rules

validateBook :: [Int] -> [(Int, Int)] -> Bool
validateBook = all . validateRule

validBooks :: [(Int, Int)] -> [[Int]] -> [[Int]]
validBooks rules = filter (`validateBook` rules)

middleElem :: [a] -> a
middleElem xs = xs !! (length xs `div` 2)

validMiddlePages :: [(Int, Int)] -> [[Int]] -> [Int]
validMiddlePages rules = map middleElem . validBooks rules

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn delim (x : xs)
  | x == delim = [] : splitOn delim xs
  | otherwise = aux (splitOn delim xs)
  where
    aux (xs' : ys) = (x : xs') : ys

parseRules :: [String] -> [(Int, Int)]
parseRules = map (bimap read (read . tail) . span (/= '|'))

parseBooks :: [String] -> [[Int]]
parseBooks = map (map read . splitOn ',')

parseInput :: String -> ([(Int, Int)], [[Int]])
parseInput = bimap parseRules (parseBooks . tail) . break null . lines

main :: IO ()
main = do
  input <- getContents
  let (rules, books) = parseInput input
  print $ "p1: " ++ show (sum (validMiddlePages rules books))
