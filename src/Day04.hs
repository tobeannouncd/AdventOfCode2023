{-# LANGUAGE OverloadedStrings #-}
module Day04 (solve) where

import qualified Data.Text as T
import Data.Text.Read (decimal)
import Data.Bits (Bits(shiftL))
import Control.Arrow ((&&&))

data Card = Card
  { winningNums :: [Int]
  , pickedNums :: [Int]
  } deriving (Show,Eq)

solve :: T.Text -> (Int,Int)
solve = (part1 &&& part2) . map parseCard . T.lines

parseCard :: T.Text -> Int
parseCard t
  | [_, t'] <- T.splitOn ": " t
  , [a, b] <- T.splitOn " | " t'
  , Right ws <- traverse decimal (T.words a)
  , Right ns <- traverse decimal (T.words b)
  = let chosen = map fst ws :: [Int]
        picked = map fst ns
        in length . filter (`elem` picked) $ chosen
parseCard _ = error "cannot parse"


part1 :: [Int] -> Int
part1 = sum . map score
  where
    score 0 = 0
    score n = shiftL 1 (n-1)

part2 :: [Int] -> Int
part2 = go (f [])
  where
    go (x:xs) (m:ms) =
      1 + x + go (zipWith (+) xs $ f (replicate m (1+x))) ms
    go _ _ = 0
    f = (++ repeat 0)