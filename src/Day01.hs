{-# LANGUAGE OverloadedStrings #-}
module Day01 (solve) where

import qualified Data.Text as T
import Data.Char (isDigit, digitToInt)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Control.Applicative ((<|>), liftA2)
import Text.Read (readMaybe)

solve :: T.Text -> IO ()
solve input = do
  let xs = T.lines input
  mapM_ (print . sum . (`map` xs) . calBy) [part1, part2]

part1,part2 :: T.Text -> [Int]
part1 = map digitToInt . filter isDigit . T.unpack
part2 = mapMaybe f . T.tails
  where
    f t = readMaybe (take 1 $ T.unpack t) <|>
          snd <$> find ((`T.isPrefixOf` t) . fst) terms
    terms = zip (T.words "one two three four five six seven eight nine") [1..]

calBy :: (T.Text -> [Int]) -> T.Text -> Int
calBy f = liftA2 (+) ((10*) . head) last . f