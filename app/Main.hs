module Main (main) where

import Data.Text (Text)
import Data.Time (LocalTime (..),ZonedTime (..),getZonedTime,toGregorian)
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Advent (defaultAoCOpts,runAoC,mkDay_,AoC(AoCInput))
import Control.Monad (guard)

import Day01 (solve)
import Day02 (solve)
import Day03 (solve)
import Day04 (solve)
import Day05 (solve)
import Day06 (solve)
import Day07 (solve)
import Day08 (solve)
import Day09 (solve)
import Day10 (solve)
import Day11 (solve)
import Day12 (solve)
import Day13 (solve)
import Day14 (solve)
import Day15 (solve)
import Day16 (solve)
import Day17 (solve)
import Day18 (solve)
import Day19 (solve)
import Day20 (solve)
import Day21 (solve)
import Day22 (solve)
import Day23 (solve)
import Day24 (solve)
import Day25 (solve)

solveFuncs :: [Text -> IO ()]
solveFuncs =
  [ Day01.solve,
    Day02.solve,
    Day03.solve,
    Day04.solve,
    Day05.solve,
    Day06.solve,
    Day07.solve,
    Day08.solve,
    Day09.solve,
    Day10.solve,
    Day11.solve,
    Day12.solve,
    Day13.solve,
    Day14.solve,
    Day15.solve,
    Day16.solve,
    Day17.solve,
    Day18.solve,
    Day19.solve,
    Day20.solve,
    Day21.solve,
    Day22.solve,
    Day23.solve,
    Day24.solve,
    Day25.solve
  ]

currentDay :: IO Int
currentDay = do
  t <- localDay . zonedTimeToLocalTime <$> getZonedTime
  let (_, _, d') = toGregorian t
  return d'

puzzleInput :: String -> Int -> IO Text
puzzleInput session day =
    either (error . show) id <$> runAoC opts (AoCInput (mkDay_ (toInteger day)))
  where
    opts = defaultAoCOpts 2023 session

main :: IO ()
main = do
  args <- getArgs
  day <- case args of
          [x] -> maybe currentDay return $ readMaybe x
          _   -> currentDay
  guard $ day > 0 && day < 26
  session <- readFile ".session"
  input <- puzzleInput session day
  (solveFuncs !! (day - 1)) input