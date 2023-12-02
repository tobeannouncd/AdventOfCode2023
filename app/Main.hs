module Main (main) where

import Data.Text (Text)
import Data.Time (LocalTime (..),ZonedTime (..),getZonedTime,toGregorian)
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Advent (defaultAoCOpts,runAoC,mkDay_,AoC(AoCInput))
import Control.Monad (guard)

import Day1 (solve)
import Day2 (solve)
import Day3 (solve)
import Day4 (solve)
import Day5 (solve)
import Day6 (solve)
import Day7 (solve)
import Day8 (solve)
import Day9 (solve)
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
  [ Day1.solve,
    Day2.solve,
    Day3.solve,
    Day4.solve,
    Day5.solve,
    Day6.solve,
    Day7.solve,
    Day8.solve,
    Day9.solve,
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