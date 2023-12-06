{-# LANGUAGE OverloadedStrings #-}
module Day05 (solve) where

import qualified Data.Text as T
import IntervalTree ( TreeD, empty, insert, lookupTree, overlap )
import Data.Text.Read (decimal)
import Control.Arrow ((&&&))

type Tree = TreeD Int (Int -> Int)

mkTree :: T.Text -> Tree
mkTree input
  | (_:xs) <- T.lines input
  = foldr (uncurry insert . parse) empty xs
  where
    parse s
      | Right [d0,s0,n] <- traverse (fmap fst . decimal) (T.words s)
      = ((s0, s0+n-1), \x -> x-s0+d0)
    parse _ = error "cannot parse line"
mkTree _ = error "cannot parse tree"

parseInput :: T.Text -> ([Tree], [Int])
parseInput input
  | (a:b) <- T.splitOn "\n\n" input
  , Right seeds <- traverse (fmap fst . decimal) (drop 1 $ T.words a)
  = (map mkTree b, seeds)
parseInput _ = error "cannot parse seeds"

part1 :: ([Tree], [Int]) -> Int
part1 (trees, seeds) = minimum . map (\x -> foldl f x trees) $ seeds
  where
    f x = maybe x ($ x) . lookupTree x

solve :: T.Text -> (Int,Int)
solve = (part1 &&& part2) . parseInput

part2 :: ([Tree], [Int]) -> Int
part2 (trees, seeds) = 
    fst . minimum . foldl (\ps -> (ps >>=) . f) (mkPairs seeds) $ trees
  where
    mkPairs (a:b:xs) = (a,a+b-1) : mkPairs xs
    mkPairs _ = []
    f tree pair = as ++ map g bs
      where
        (as, bs) = overlap pair tree
    g ((a,b), h) = (h a, h b)
