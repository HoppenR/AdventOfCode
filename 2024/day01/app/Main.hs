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

parseInput :: String -> [[Int]]
parseInput input =
  transpose $ map (map read . words) (lines input)

main :: IO ()
main = do
  input <- getContents
  let [xs, ys] = map sort $ parseInput input
  print $ "p1: " ++ show (listDist xs ys)
  print $ "p2: " ++ show (similarityScore (Map.fromListWith (+) (zip ys (repeat 1))) xs)
