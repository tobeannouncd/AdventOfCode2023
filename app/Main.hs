module Main (main) where

import System.Environment (getArgs)

import Data.Text (Text)
import Data.Typeable (cast, Typeable)
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

solveFuncs :: [Text -> (String, String)]
solveFuncs = 
  [ showBoth . Day01.solve,
    showBoth . Day02.solve,
    showBoth . Day03.solve,
    showBoth . Day04.solve,
    showBoth . Day05.solve,
    showBoth . Day06.solve ]

showBoth :: (Show a1, Show a2, Typeable a1, Typeable a2) 
         => (a1, a2) -> (String, String)
showBoth (a,b) = (show_ a, show_ b)

show_ :: (Show a, Typeable a) => a -> String
show_ x = case cast x of
            Nothing -> show x
            Just s  -> s

est :: IO TZ
est = loadSystemTZ "America/New_York"

currentDay :: IO Int
currentDay = do
  tz <- est
  (_,_,d) <- toGregorian . localDay . utcToLocalTimeTZ tz . zonedTimeToUTC 
    <$> getZonedTime
  return $ min d 25

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