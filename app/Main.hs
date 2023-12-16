module Main (main) where

import System.Environment (getArgs)

import Data.Text (Text)
import Data.Typeable (cast, Typeable)

import Data.Time
  ( LocalTime (..), getZonedTime, toGregorian, zonedTimeToUTC, DayOfMonth )
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

solveFuncs :: [Text -> (String, String)]
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
    showBoth . Day16.solve ]

showBoth :: (Show a1, Show a2, Typeable a1, Typeable a2)
         => (a1, a2) -> (String, String)
showBoth (a,b) = (show_ a, show_ b)
  where
    show_ x = case cast x of
                Nothing -> show x
                Just s  -> s

est :: IO TZ
est = loadSystemTZ "America/New_York"

currentDay :: IO DayOfMonth
currentDay = do
  tz <- est
  (_,m,d) <- toGregorian . localDay . utcToLocalTimeTZ tz . zonedTimeToUTC
    <$> getZonedTime
  return $ if m == 12 then min d 25 else 1

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

runSolution :: DayOfMonth -> IO (String,String)
runSolution day = do
  session <- readFile ".session"
  solveFuncs !! (day-1) <$> puzzleInput session day

main :: IO ()
main = do
  args <- getArgs
  day  <- case args of
           []  -> currentDay
           [x] -> readIO x
           _   -> error "invalid flags"
  runSolution day >>= printSolution