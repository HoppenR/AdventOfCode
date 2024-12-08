-- Vim: set sw=2 :
module Main where

import Data.Bifunctor (bimap)
import Data.Graph (graphFromEdges, topSort)
import Data.List (partition)

validateRule :: [Int] -> (Int, Int) -> Bool
validateRule [] _ = True
validateRule (x : xs) rules@(rule1, rule2)
  | x == rule1 = True
  | x == rule2 = rule1 `notElem` xs
  | otherwise = validateRule xs rules

partitionBooks :: [(Int, Int)] -> [[Int]] -> ([[Int]], [[Int]])
partitionBooks rules = partition (`validBook` rules)
  where
    validBook = all . validateRule

sortBook :: [(Int, Int)] -> [Int] -> [Int]
sortBook rules book =
  map (pageFromNode . nodeFromVertex) (topSort graph)
  where
    pageFromNode (_, page, _) = page
    (graph, nodeFromVertex, _) = graphFromEdges edgeList
    edgeList = [(page, page, successors page) | page <- book]
    successors page = [p2 | (p1, p2) <- rules, p1 == page]

middleElem :: [a] -> a
middleElem xs = xs !! (length xs `div` 2)

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
  let (validBooks, invalidBooks) = partitionBooks rules books
  print $ "p1: " ++ show (sum (map middleElem validBooks))
  print $ "p2: " ++ show (sum (map (middleElem . sortBook rules) invalidBooks))
