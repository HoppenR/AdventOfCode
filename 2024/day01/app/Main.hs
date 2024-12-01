-- Vim: set sw=2 :
module Main where

import Data.List (sort, transpose)
import qualified Data.Map as Map
import System.IO

similarityScore :: Map.Map Int Int -> [Int] -> Int
similarityScore left =
  foldl (\acc x -> acc + x * Map.findWithDefault 0 x left) 0

listDist :: [Int] -> [Int] -> Int
listDist xs ys =
  sum $ zipWith (\x y -> abs (x - y)) xs ys

mapOccurrences :: [Int] -> Map.Map Int Int
mapOccurrences = foldr (\x acc -> Map.insertWith (+) x 1 acc) Map.empty

parseInput :: String -> [[Int]]
parseInput input =
  transpose $ map (map read . words) (lines input)

main :: IO ()
main = do
  input <- getContents
  let [xs, ys] = map sort $ parseInput input
  print $ listDist xs ys
  print $ similarityScore (mapOccurrences ys) xs
