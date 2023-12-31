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
findMirror n = (\(a,b) -> 100*a+b) . (go [] &&& go [] . transpose)
  where
    go acc (x:y:xs)
      | hamming x y <= n,
        sum (zipWith hamming (x:acc) (y:xs)) == n = length (x:acc)
    go acc (x:xs) = go (x:acc) xs
    go _ _ = 0

hamming :: Eq b => [b] -> [b] -> Int
hamming xs = length . filter id . zipWith (/=) xs
