{-# LANGUAGE OverloadedStrings #-}
module Day06 (solve) where

import qualified Data.Text as T
import Data.Text.Read (decimal)
import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)
import Data.List (find)

solve :: T.Text -> (Int,Int)
solve = (part1 &&& part2) . parse

parse :: T.Text -> [(Int,Int)]
parse input
  | [t,d] <- T.lines input
  = zip (ints t) (ints d)
parse _ = error "cannot parse"

ints :: Integral a => T.Text -> [a]
ints t
  | T.null t = []
  | otherwise = case decimal t of
      Left _ -> ints (T.tail t)
      Right (x,t') -> x : ints t'

part1 :: [(Int, Int)] -> Int
part1 = product . map (uncurry countWays)

countWays :: Int -> Int -> Int
countWays time record = fromMaybe 0 $ do
    fwd <- f [0..time]
    rev <- f [time, time-1 .. 0]
    return $ rev - fwd + 1
  where
    f = find (\s -> s * (time-s) > record)


part2 :: [(Int,Int)] -> Int
part2 inp = countWays time record
  where
    (times,records) = unzip inp
    time = con times
    record = con records
    con = read . concatMap show