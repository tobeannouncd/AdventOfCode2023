{-# LANGUAGE TupleSections #-}
module Day03 (solve) where

import Data.Text (Text, unpack)
import Control.Arrow ( Arrow((&&&)) )
import Data.Char ( isDigit )
import Data.List (intersect, groupBy, union, (\\))
import Data.Function (on)
import Data.Map (Map, fromList, findWithDefault, assocs)

process :: String -> ([Val], Map (Int, Int) Char)
process = findNums &&& makeMap

type Val = (Int, [(Int,Int)])

findNums :: String -> [Val]
findNums s = concat [findVals y row | (y,row) <- zip [0..] (lines s)]
  where
    findVals y row = map (f y) . filter (isDigit.snd.head)$ groupBy ((==) `on` (isDigit . snd)) $ zip [0..] row
    f y vals = let
      (xs, ns) = unzip vals
      in (read ns, map (,y) xs)

makeMap :: String -> Map (Int,Int) Char
makeMap s = fromList
  [ ((x,y),val)
  | (y,row) <- zip [0..] (lines s)
  , (x,val) <- zip [0..] row]

adjacent :: (Int,Int) -> [(Int,Int)]
adjacent (x,y) =
  [ (xx,yy)
  | xx <- [x-1 .. x+1]
  , yy <- [y-1 .. y+1]
  , (xx,yy) /= (x,y) ]

adjacents :: [(Int,Int)] -> [(Int,Int)]
adjacents xys = foldr (union . adjacent) [] xys \\ xys

solve :: Text -> (Int,Int)
solve = (part1 &&& part2) . process . unpack

part2 :: ([Val], Map (Int, Int) Char) -> Int
part2 (vals, m) = sum
    [ product xs
    | (pt,'*') <- assocs m
    , let xs = adjVals pt
    , length xs == 2 ]
  where
    adjVals pt = let adj = adjacent pt in
      [ v
      | (v, pts) <- vals
      , (not . null) $ intersect pts adj ]
      

part1 :: ([Val], Map (Int, Int) Char) -> Int
part1 (vals, m) = sum . map fst $ filter (f.snd) vals
  where
    f xys = let
      adj = adjacents xys
      syms = filter (/='.') $ map (\xy -> findWithDefault '.' xy m) adj
      in not $ null syms
