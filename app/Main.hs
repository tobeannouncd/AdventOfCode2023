module Main (main) where

import System.Environment (getArgs)

import Data.Text (Text)
import Text.Read (readMaybe)

import Data.Time (LocalTime (..), getZonedTime, toGregorian, zonedTimeToUTC)
import Data.Time.Zones (utcToLocalTimeTZ, loadSystemTZ, TZ)

import Advent (defaultAoCOpts, runAoC, mkDay_, AoC(AoCInput))

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

est :: IO TZ
est = loadSystemTZ "America/New_York"

currentDay :: IO Int
currentDay = do
  tz <- est
  t <- localDay . utcToLocalTimeTZ tz . zonedTimeToUTC <$> getZonedTime
  let (_, _, d') = toGregorian t
  return $ min d' 25

_checkTime :: IO ()
_checkTime = do
  tz <- est
  nowUTC <- zonedTimeToUTC <$> getZonedTime
  print $ utcToLocalTimeTZ tz nowUTC

puzzleInput :: String -> Int -> IO Text
puzzleInput session day = either (error . show) id
  <$> runAoC (defaultAoCOpts 2023 session) (AoCInput (mkDay_ (toInteger day)))

main :: IO ()
main = do
  args <- getArgs
  day <- case args of
          [x] -> maybe currentDay (return . max 1 . min 25) $ readMaybe x
          _   -> currentDay
  session <- readFile ".session"
  input <- puzzleInput session day
  (solveFuncs !! (day - 1)) input