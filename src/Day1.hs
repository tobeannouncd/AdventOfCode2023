{-# LANGUAGE OverloadedStrings #-}
module Day1 (solve) where

import qualified Data.Text as T
import Data.Char (isDigit, digitToInt)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Control.Applicative ((<|>))
import Text.Read (readMaybe)

solve :: T.Text -> IO ()
solve input = do
  let xs = T.lines input
  mapM_ (print . sum . (`map` xs) . calBy) [part1, part2]

part1,part2 :: T.Text -> [Int]
part1 = map digitToInt . T.unpack . T.filter isDigit
part2 = mapMaybe f . init . T.tails
  where
    f t = readMaybe (T.unpack $ T.take 1 t) <|>
          snd <$> find (\(s,_) -> s `T.isPrefixOf` t) terms
    terms = zip (T.words "one two three four five six seven eight nine") [1..]

calBy :: (T.Text -> [Int]) -> T.Text -> Int
calBy f line = 10*head digits + last digits
  where
    digits = f line