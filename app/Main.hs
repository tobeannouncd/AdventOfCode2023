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

solveFuncs :: [Text -> String]
solveFuncs = 
  [ show . Day01.solve,
    show . Day02.solve,
    show . Day03.solve,
    show . Day04.solve,
    show . Day05.solve,
    show . Day06.solve,
    show . Day07.solve,
    show . Day08.solve,
    show . Day09.solve,
    show . Day10.solve,
    show . Day11.solve,
    show . Day12.solve,
    show . Day13.solve,
    show . Day14.solve,
    show . Day15.solve,
    show . Day16.solve,
    show . Day17.solve,
    show . Day18.solve,
    show . Day19.solve,
    show . Day20.solve,
    show . Day21.solve,
    show . Day22.solve,
    show . Day23.solve,
    show . Day24.solve,
    show . Day25.solve
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
  putStrLn $ (solveFuncs !! (day - 1)) input