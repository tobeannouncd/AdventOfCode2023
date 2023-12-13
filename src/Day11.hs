{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day11 (solve) where

import Data.Text (Text, unpack)
import Control.Arrow ((***))
import Lattice
    ( Pt(..), ptLines, ptRow, ptCol, manhattan, boundingBox )
import qualified Data.Set as Set

type Output = (Int,Int)

solve :: Text -> Output
solve inp = (f *** f) (2, 1000000)
  where
    f k = sum $ map (distance gaps k) pairs
    galaxies = [pt | (pt,'#') <- ptLines (unpack inp)]
    gaps = findGaps galaxies
    pairs = mkPairs galaxies
    mkPairs [] = []
    mkPairs (x:xs) = map (x,) xs ++ mkPairs xs

distance :: (Set.Set Int, Set.Set Int) -> Int -> (Pt, Pt) -> Int
distance (yGaps, xGaps) k (a@(Pt y1 x1), b@(Pt y2 x2))
  = manhattan a b + (k-1) * (gaps yGaps ys + gaps xGaps xs)
  where
    ys = if y1 <= y2 then [y1 .. y2] else [y2 .. y1]
    xs = if x1 <= x2 then [x1 .. x2] else [x2 .. x1]
    gaps s lst = length $ filter (`Set.member` s) lst

findGaps :: [Pt] -> (Set.Set Int, Set.Set Int)
findGaps galaxies = yGaps `seq` xGaps `seq` (yGaps, xGaps)
  where
    Just (Pt yLo xLo, Pt yHi xHi) = boundingBox galaxies
    yVals = Set.fromList $ map ptRow galaxies
    xVals = Set.fromList $ map ptCol galaxies
    yGaps = Set.fromList [yLo .. yHi] `Set.difference` yVals
    xGaps = Set.fromList [xLo .. xHi] `Set.difference` xVals