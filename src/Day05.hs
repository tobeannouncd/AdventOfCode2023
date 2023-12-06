{-# LANGUAGE OverloadedStrings #-}
module Day05 (solve) where

import qualified Data.Text as T
import Control.Arrow (Arrow(..))
import Data.Text.Read (decimal)

data MapItem
  = MapItem
  { dst :: Int
  , src :: Int
  , len :: Int
  } deriving (Show,Eq)

solve :: T.Text -> (Int,Int)
solve input = (part1 maps &&& part2 maps) seeds
  where
    (seeds, maps) = parse input

parse :: Integral b => T.Text -> ([b], [[MapItem]])
parse input
  | (a:b) <- T.splitOn "\n\n" input
  , (_:xs) <- T.words a
  , Right seeds <- traverse (fmap fst . decimal) xs
  = (seeds, map (map parseItem . tail . T.lines) b)
parse _ = error "cannot parse"

parseItem :: T.Text -> MapItem
parseItem line
  | Right [a,b,c] <- traverse (fmap fst . decimal) (T.words line)
  = MapItem a b c
parseItem _ = error "cannot parse"

part1 :: Foldable t => t [MapItem] -> [Int] -> Int
part1 maps = minimum . map (\x -> foldl mapRange x maps)

mapRange :: Int -> [MapItem] -> Int
mapRange x [] = x
mapRange x (MapItem d s l:xs)
  | s <= x && x < s + l = d + x - s
  | otherwise = mapRange x xs

part2 :: Foldable t => t [MapItem] -> [Int] -> Int
part2 maps seeds = fst . minimum . foldl f (toPairs seeds) $ maps
  where
    f xs ms = xs >>= (`mapRange'` ms)
    toPairs (x:y:xs) = (x,y) : toPairs xs
    toPairs _ = []

mapRange' :: (Int, Int) -> [MapItem] -> [(Int, Int)]
mapRange' x [] = [x]
mapRange' (rs,rl) (MapItem d s l:ms)
  | rs <= s + l && s < rs + rl = pre ++ cur ++ suf
  | otherwise = mapRange' (rs,rl) ms
  where
    pre = if rs < s then mapRange' (rs,s-rs) ms else []
    cur = [(d + max 0 (rs-s), min rl (l - max 0 (rs-s)))]
    suf = if s+l < rs + rl then mapRange' (s+l, rs+rl-s-l) ms else []

