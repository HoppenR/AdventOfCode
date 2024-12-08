-- Vim: set sw=2 :
module Main where

import Control.Monad (replicateM)

data Operator = Add | Mult | Concat

opPerms :: [Operator] -> Int -> [[Operator]]
opPerms ops = (`replicateM` ops)

numCount :: Int -> Int
numCount = (+ 1) . floor . logBase 10 . fromIntegral

evalStep :: Int -> (Operator, Int) -> Int
evalStep acc (Add, num) = acc + num
evalStep acc (Mult, num) = acc * num
evalStep acc (Concat, num) = acc * 10 ^ numCount num + num

evalPerm :: [Int] -> [Operator] -> Int
evalPerm (n1 : nums) ops = foldl evalStep n1 (zip ops nums)

evalLine :: [Operator] -> (Int, [Int]) -> Bool
evalLine ops (res, nums) = any ((== res) . evalPerm nums) (opPerms ops (length nums - 1))

parseInput :: String -> [(Int, [Int])]
parseInput = map (parseLine . words) . lines
  where
    parseLine (hd : tl) = (read (init hd), map read tl)

main :: IO ()
main = do
  input <- getContents
  let calcLines = parseInput input
  print $ "p1: " ++ show (sum (map fst (filter (evalLine [Add, Mult]) calcLines)))
  print $ "p2: " ++ show (sum (map fst (filter (evalLine [Add, Mult, Concat]) calcLines)))
