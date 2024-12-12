-- Vim: set sw=2 :
module Main where

import Data.Char (ord)

data DiskSpan
  = File Int Int -- ID range
  | Free Int -- range

lastWhere :: (DiskSpan -> Bool) -> [DiskSpan] -> (DiskSpan, [DiskSpan])
lastWhere pred xs = go [] (reverse xs)
  where
    go acc (x : xs)
      | pred x = (x, reverse (acc ++ xs))
      | otherwise = go (x : acc) xs

isFragmented :: [DiskSpan] -> Bool
isFragmented disk = length (filter isFree disk) > 1
  where
    isFree span = case span of Free _ -> True; _ -> False

mergeFree :: [DiskSpan] -> [DiskSpan]
mergeFree [] = []
mergeFree (Free m : Free n : xs) = mergeFree (Free (m + n) : xs)
mergeFree (x : xs) = x : mergeFree xs

defragOne :: DiskSpan -> [DiskSpan] -> ([DiskSpan], DiskSpan)
defragOne file [] = ([], file)
defragOne file@(File fileId fiSpan) (free@(Free frSpan) : rest)
  | fiSpan == frSpan = (file : rest, free)
  | fiSpan < frSpan = (file : Free (frSpan - fiSpan) : rest, free)
  | otherwise =
      let (files', Free freed) = defragOne (File fileId (fiSpan - frSpan)) (File fileId frSpan : rest)
       in (files', Free (freed + frSpan))
defragOne file (x : rest) =
  let (rest', remain) = defragOne file rest
   in (x : rest', remain)

moveFile :: [DiskSpan] -> [DiskSpan]
moveFile disk =
     mergeFree (disk'' ++ [remain])
  where
    (disk'', remain) = defragOne lastFile disk'
    (lastFile, disk') = lastWhere isFile disk
    isFile span = case span of File _ _ -> True; _ -> False

defragmentFull :: [DiskSpan] -> [DiskSpan]
defragmentFull disk
  | isFragmented disk = defragmentFull (moveFile disk)
  | otherwise = disk

countScore :: [DiskSpan] -> Int
countScore = go 0 0
  where
    go :: Int -> Int -> [DiskSpan] -> Int
    go acc i (File id 0 : xs) = go acc i xs
    go acc i (File id span : xs) = go (acc + id * i) (i + 1) (File id (span - 1) : xs)
    go acc i [Free _] = acc

parseInput :: String -> [DiskSpan]
parseInput = readFile 0
  where
    readFree _ ['\n'] = []
    readFree id (hd : tl) = Free (ord hd - ord '0') : readFile id tl
    readFile id (hd : tl) = File id (ord hd - ord '0') : readFree (id + 1) tl

main :: IO ()
main = do
  input <- getContents
  let calcLines = parseInput input
  print $ "p1: " ++ show (countScore (defragmentFull calcLines))
