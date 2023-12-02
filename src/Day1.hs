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
  print (sum $ map calVal  $ T.lines input)
  print (sum $ map calVal' $ T.lines input)

calVal :: T.Text -> Int
calVal line = 10 * head digits + last digits
  where
    digits = map digitToInt . T.unpack $ T.filter isDigit line

calVal' :: T.Text -> Int
calVal' line = 10 * head digs + last digs
  where
    digs  = mapMaybe f $ init $ T.tails line
    terms = zip (T.words "one two three four five six seven eight nine") [1..]

    f s = 
      readMaybe (T.unpack $ T.take 1 s) <|> 
      snd <$> find (\(t,_) -> t `T.isPrefixOf` s) terms