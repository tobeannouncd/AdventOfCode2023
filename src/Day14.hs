module Day14 (solve) where

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List (findIndices, groupBy, sort, transpose)
import Data.Set qualified as S
import Data.Text (Text, unpack)

type Output = (Int, Int)

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
load = snd . foldr f (1, 0)
  where
    f xs (i, acc) = (i + 1, acc + i * length (filter (== 'O') xs))

spin :: [[Char]] -> [[Char]]
spin = east . south . west . north

indexCycle :: (Ord a) => Int -> [a] -> a
indexCycle = indexCycleBy id

indexCycleBy :: (Ord b) => (a -> b) -> Int -> [a] -> a
indexCycleBy repr n arr = go S.empty n arr
  where
    go _ _ [] = error "index too large"
    go _ 0 (x : _) = x
    go seen i (x : xs)
      | repr x `S.member` seen,
        (a : b : _) <- findIndices (\y -> repr y == repr x) arr =
          let cycleLen = b - a
              j = a + i `mod` cycleLen
           in arr !! j
      | otherwise = go (S.insert (repr x) seen) (i - 1) xs