-- Vim: set sw=2 :
module Main where

import Text.ParserCombinators.Parsec

data Instr
  = Mul Int Int
  | Do
  | Dont
  deriving (Show, Eq)

mulParser :: Parser Instr
mulParser = do
  char '('
  d1 <- many1 digit
  char ','
  d2 <- many1 digit
  char ')'
  return $ Mul (read d1) (read d2)

parseInput :: String -> [Instr]
parseInput [] = []
parseInput ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : tl) =
  Dont : parseInput tl
parseInput ('d' : 'o' : '(' : ')' : tl) =
  Do : parseInput tl
parseInput ('m' : 'u' : 'l' : tl) =
  case parse mulParser "input" tl of
    Right result -> result : parseInput tl
    Left _ -> parseInput tl
parseInput input = parseInput (tail input)

filterEnabled :: [Instr] -> [Instr]
filterEnabled [] = []
filterEnabled (Dont : xs) = filterEnabled (dropWhile (/= Do) xs)
filterEnabled (x : xs) = x : filterEnabled xs

main :: IO ()
main = do
  input <- getContents
  let instrs = parseInput input
  print $ "p1: " ++ show (sum [l * r | Mul l r <- instrs])
  print $ "p2: " ++ show (sum [l * r | Mul l r <- filterEnabled instrs])
