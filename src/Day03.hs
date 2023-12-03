{-# LANGUAGE TupleSections #-}
module Day03 (solve) where

import Data.Text (Text, unpack)
import Control.Arrow ( Arrow((&&&)) )
import Data.Char ( isDigit )
import Data.List (intersect)


solve :: Text -> (Int,Int)
solve = (part1 &&& part2) . lines . unpack


(!) :: [[a]] -> (Int,Int) -> a
xss ! (i,j) = xss !! i !! j

part1 :: [String] -> Int
part1 grid = sum
    [ num
    | (pts,num) <- findNums grid
    , any (isSym . (grid !)) (adjAll pts)]
  where
    isSym x = x `notElem` ('.':['0'..'9'])
    adjAll pts = [p | pt <- pts, p <- adj pt, p `notElem` pts]
    adj (i,j) = [(i+di,j+dj) | di <- [-1 .. 1], dj <- [-1 .. 1], (di,dj) /= (0,0),
      i+di >= 0, i+di < rows, j+dj >= 0, j+dj < cols]
    rows = length grid
    cols = length (head grid)

    
findNums :: [String] -> [([(Int,Int)],Int)]
findNums grid = 
    [ (map (i,) js, num) 
    | (i,row) <- zip [0..] grid
    , (js, num) <- findNums' $ zip [0..] row ]
  where
    findNums' [] = []
    findNums' xs@((_,x):_)
      | isDigit x = let (a,b) = span (isDigit.snd) xs
                        (is, ns) = unzip a
                        in (is, read ns):findNums' b
      | otherwise = findNums' $ dropWhile (not . isDigit . snd) xs


part2 :: [String] -> Int
part2 grid = sum gears
  where
    ipts = findNums grid
    adj (i,j) = [(ii,jj) | ii <- [i-1 .. i+1], jj <- [j-1 .. j+1]
      , (ii,jj) /= (i,j)]
    gears =
      [ product (map snd xs)
      | (i,row) <- zip [0..] grid
      , (j,'*') <- zip [0..] row
      , let adj' = adj (i,j)
      , let xs = filter (\(pts,_) -> not $ null $ intersect pts adj') ipts
      , length xs == 2]
