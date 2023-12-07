{-# LANGUAGE OverloadedStrings #-}
module Day02 (solve) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map, fromList, isSubmapOfBy, elems, unionsWith)
import Control.Arrow ((&&&))
import Data.Either (fromRight)
import Data.Char (isDigit, isLetter)
import TextParsing
    ( Parser, char, satisfy, string, many1, sepBy1, parse )

type Counter = Map Text Int

solve :: Text -> (Int,Int)
solve = (part1 &&& part2) . map parseLine . T.lines

part1 :: [(Int, [Counter])] -> Int
part1 = sum . map fst . filter (all valid . snd)
  where
    inv = fromList
      [ ("red", 12)
      , ("green", 13)
      , ("blue", 14) ]

    valid s = isSubmapOfBy (<=) s inv

part2 :: [(a, [Counter])] -> Int
part2 = sum . map (product . elems . unionsWith max . snd)

parseLine :: Text -> (Int, [Counter])
parseLine = fromRight (error "cannot parse") . parse line ""

line :: Parser (Int, [Counter])
line = do
    g <- game
    _ <- string ": "
    xs <- sepBy1 counter (string "; ")
    return (g, xs)
  where
    game = do
      _ <- string "Game "
      read <$> many1 (satisfy isDigit)
    counter = fromList <$> sepBy1 item (string ", ")
    item = do
      ds <- many1 (satisfy isDigit)
      _ <- char ' '
      xs <- many1 (satisfy isLetter)
      return (T.pack xs, read ds)
