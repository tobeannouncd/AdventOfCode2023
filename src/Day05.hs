{-# LANGUAGE OverloadedStrings #-}
module Day05 (solve) where

import Data.Text (Text, splitOn)
import Data.IntervalMap.FingerTree
    ( empty,
      high,
      insert,
      intersections,
      low,
      search,
      Interval(..),
      IntervalMap )
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Control.Arrow ( Arrow((&&&)) )
import Data.List (sort)

solve :: Text -> (Int,Int)
solve = (part1 &&& part2) . parseInput

part1 :: (Ord c, Foldable t) => ([c], t (IntervalMap c (c -> c))) -> c
part1 (seeds, trees) = minimum . map (\s -> foldl getVal s trees) $ seeds
  where
    getVal s tree = case search s tree of
      [] -> s
      ((_,f):_) -> f s

part2 :: (Foldable t, Integral a) => ([a], t (IntervalMap a (a -> a))) -> a
part2 (seeds, trees) = minimum . map low
                     . foldl (\ps t -> merge . sort $ ps >>= translate t) pairs
                     $ trees
  where
    pairs = go seeds
    go [] = []
    go (a:b:xs) = Interval a (a + b - 1) : go xs
    go _ = undefined

merge :: Ord a => [Interval a] -> [Interval a]
merge (x:y:xs)
  | high x < low y = x : merge (y:xs)
  | otherwise = merge (Interval (low x) (max (high x) (high y)):xs)
merge xs = xs

translate :: Integral a => IntervalMap a (a -> a) -> Interval a -> [Interval a]
translate tree i = uncurry (++) $ foldr overlap ([], [i]) $ intersections i tree

overlap :: Integral a => (Interval a, a -> a)
                      -> ([Interval a],[Interval a])
                      -> ([Interval a],[Interval a])
overlap (Interval a b, f) (done, unmatched) = foldr check (done,[]) unmatched
  where
    check i@(Interval a' b') (gd, bd)
      | valid curr = (curr:gd, filter valid [prev,post] ++ bd)
      | otherwise = (gd, i:bd)
      where
        prev = Interval a' (min b' (a-1))
        curr = Interval (f $ max a a') (f $ min b b')
        post = Interval (max (b+1) a') b'
    valid i = low i <= high i


parseInput :: (Integral a, Integral v) => Text -> ([a], [IntervalMap v (v -> v)])
parseInput t
  | (a:b) <- splitOn "\n\n" t
  = (readInts a, map mkMap b)
parseInput _ = undefined

readInts :: Integral a => Text -> [a]
readInts t
  | T.null t = []
  | otherwise = case decimal t of
      Left _ -> readInts (T.tail t)
      Right (x,t') -> x : readInts t'

mkMap :: Integral v => Text -> IntervalMap v (v -> v)
mkMap t
  | (_:xs) <- T.lines t
  = foldr (uncurry insert . f . readInts) empty xs
  where
    f [d0, s0, n] = (Interval s0 (s0 + n - 1), \x -> x - s0 + d0)
    f _ = undefined
mkMap _ = undefined
