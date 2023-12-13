module Day09 (solve) where

import Data.Text (Text)
import qualified Data.Text as T
import Utils ( readInts )

type Output = (Integer,Integer)

solve :: Text -> Output
solve input = (sum (map extrapolate vals), sum (map extrapLeft vals))
  where
    vals = map readInts (T.lines input)

extrapolate :: [Integer] -> Integer
extrapolate xs
  | all (==0) xs = 0
  | otherwise = last xs + extrapolate xs'
  where
    xs' = zipWith (-) (tail xs) xs


extrapLeft :: [Integer]  -> Integer
extrapLeft xs
  | all (==0) xs = 0
  | otherwise = head xs - extrapLeft xs'
  where
    xs' = zipWith (-) (tail xs) xs
