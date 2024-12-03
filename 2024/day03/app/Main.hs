-- Vim: set sw=2 :
module Main where

import Text.ParserCombinators.Parsec

mulParser :: Parser (Int, Int)
mulParser = do
  string "mul"
  char '('
  d1 <- many1 digit
  char ','
  d2 <- many1 digit
  char ')'
  return (read d1, read d2)

parseInput :: String -> [(Int, Int)]
parseInput input =
  case input of
    "" -> []
    _ -> case parse mulParser "input" input of
      Left _ -> parseInput (tail input)
      Right result -> result : parseInput (tail input)

main :: IO ()
main = do
  input <- getContents
  let mulArgs = parseInput input
  print $ "p1: " ++ show (sum (map (uncurry (*)) mulArgs))
