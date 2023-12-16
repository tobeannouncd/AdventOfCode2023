module Day09 (solve) where

import Control.Arrow ((&&&))
import Data.Text (Text)
import Data.Text qualified as T
import Utils (readInts)

type Output = (Int, Int)

solve :: Text -> Output
solve = (sum . map (extrapWith last) &&& sum . map (extrapWith head)) . map readInts . T.lines

extrapWith :: (Num a, Eq a) => ([a] -> a) -> [a] -> a
extrapWith f xs
  | all (== 0) xs = 0
  | otherwise = f xs - extrapWith f xs'
  where
    xs' = zipWith (-) (tail xs) xs
