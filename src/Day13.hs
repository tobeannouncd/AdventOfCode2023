module Day13 (solve) where

import Data.Text (Text, unpack)
import Control.Arrow ( Arrow((&&&)) )
import Data.List (transpose)

type Output = (Int,Int)

solve :: Text -> Output
solve = (part1 &&& part2) . paragraphs . lines . unpack

paragraphs :: Foldable t => [t a] -> [[t a]]
paragraphs [] = []
paragraphs xs =
  let (a,b) = break null xs
  in a : paragraphs (drop 1 b)

part1 :: [[[Char]]] -> Int
part1 = sum . map (findMirror 0)

part2 :: [[[Char]]] -> Int
part2 = sum . map (findMirror 1)

findMirror :: Eq b => Int -> [[b]] -> Int
findMirror n xs =
  let byRow = findMirror' n [] xs
      byCol = findMirror' n [] (transpose xs)
  in 100*byRow + byCol

hamming :: Eq b => [b] -> [b] -> Int
hamming xs = length . filter id . zipWith (/=) xs

findMirror' :: Eq b => Int -> [[b]] -> [[b]] -> Int
findMirror' n acc (x:y:xs)
  | hamming x y <= n =
      if sum (zipWith hamming (x:acc) (y:xs)) == n
        then length (x:acc)
        else findMirror' n (x:acc) (y:xs)
findMirror' n acc (x:xs) = findMirror' n (x:acc) xs
findMirror' _ _ _ = 0