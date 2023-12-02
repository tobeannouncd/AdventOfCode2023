module Main (main) where

import System.Environment (getArgs)

import Data.Bifunctor (Bifunctor(bimap))
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

solveFuncs :: [Text -> (String,String)]
solveFuncs = 
  [ showBoth . Day01.solve,
    showBoth . Day02.solve,
    showBoth . Day03.solve,
    showBoth . Day04.solve,
    showBoth . Day05.solve,
    showBoth . Day06.solve,
    showBoth . Day07.solve,
    showBoth . Day08.solve,
    showBoth . Day09.solve,
    showBoth . Day10.solve,
    showBoth . Day11.solve,
    showBoth . Day12.solve,
    showBoth . Day13.solve,
    showBoth . Day14.solve,
    showBoth . Day15.solve,
    showBoth . Day16.solve,
    showBoth . Day17.solve,
    showBoth . Day18.solve,
    showBoth . Day19.solve,
    showBoth . Day20.solve,
    showBoth . Day21.solve,
    showBoth . Day22.solve,
    showBoth . Day23.solve,
    showBoth . Day24.solve,
    showBoth . Day25.solve
  ]

showBoth :: (Show a, Show b) => (a, b) -> (String, String)
showBoth = bimap show show

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

printSolution :: (String,String) -> IO ()
printSolution (part1, part2) = printSol '1' part1 >> printSol '2' part2
  where
    printSol n s = do
      putStrLn $ "\nPart " ++ n : "\n======"
      putStrLn s

main :: IO ()
main = do
  args <- getArgs
  day <- case args of
          [x] -> maybe currentDay (return . max 1 . min 25) $ readMaybe x
          _   -> currentDay
  session <- readFile ".session"
  input <- puzzleInput session day
  printSolution $ (solveFuncs !! (day - 1)) input