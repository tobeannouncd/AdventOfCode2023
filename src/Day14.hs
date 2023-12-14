module Day14 (solve) where

import Data.Text (Text, unpack)
import Control.Arrow ((&&&))
import Data.List ( transpose, sort, groupBy, elemIndices )
import Data.Function (on)
import qualified Data.Set as S

type Output = (Int,Int)

solve :: Text -> Output
solve = (part1 &&& part2) . lines . unpack

part1 :: [[Char]] -> Int
part1 = load . north

part2 :: [[Char]] -> Int
part2 = load . indexCycle 1000000000 . iterate spin

east :: [[Char]] -> [[Char]]
east = map shift

west :: [[Char]] -> [[Char]]
west = map (reverse . shift . reverse)

north :: [[Char]] -> [[Char]]
north = transpose . west . transpose

south :: [[Char]] -> [[Char]]
south = transpose . east . transpose

shift :: [Char] -> [Char]
shift = concatMap sort . groupBy ((==) `on` (== '#'))

load :: [[Char]] -> Int
load = snd . foldr f (1,0)
  where
    f xs (i,acc) = (i+1, acc + i * length (filter (== 'O') xs))

spin :: [[Char]] -> [[Char]]
spin = east . south . west . north

indexCycle :: Ord a => Int -> [a] -> a
indexCycle n arr = go S.empty n arr
  where
    go _ _ [] = error "index too large"
    go _ 0 (x:_) = x
    go seen i (x:xs)
      | x `S.member` seen, (a:b:_) <- elemIndices x arr =
          let cycleLen = b - a
              j = a + i `mod` cycleLen
          in arr !! j
      | otherwise = go (S.insert x seen) (i-1) xs