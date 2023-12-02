{-# LANGUAGE OverloadedStrings #-}
module Day1 (solve) where

import qualified Data.Text as T
import Data.Char (isDigit)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Control.Applicative ((<|>))
import Text.Read (readMaybe)

solve :: T.Text -> IO ()
solve input = mapM_ (\f -> print . sum $ map f $ T.lines input) 
                [calVal, calVal']

calVal :: T.Text -> Int
calVal line = read [T.head digits, T.last digits]
  where
    digits = T.filter isDigit line

calVal' :: T.Text -> Int
calVal' line = 10 * head digs + last digs
  where
    digs  = mapMaybe f $ init $ T.tails line
    terms = zip (T.words "one two three four five six seven eight nine") [1..]

    f s = 
      readMaybe (T.unpack $ T.take 1 s) <|> 
      snd <$> find (\(t,_) -> t `T.isPrefixOf` s) terms