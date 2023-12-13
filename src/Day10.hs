{-# LANGUAGE LambdaCase #-}
module Day10 (solve) where

import Data.Text (Text, unpack)
import Lattice
    ( Pt,
      ptLines,
      invert,
      invert',
      turnLeft,
      north,
      south,
      west,
      cardinal )
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set

type Output = (Int,Int)

solve :: Text -> Output
solve input = (length route `div` 2, length inLoop)
  where
    grid = Map.fromList $ ptLines (unpack input)
    (start, dir) = findStart grid
    route = [(pos,d) | (d,pos) <- dfs snd (step grid) [(dir,dir+start)]]
    pipes = Set.fromList $ map fst route
    notPipe = Map.keysSet grid `Set.difference` pipes
    candidates = Set.fromList (route >>= onLeft grid) `Set.difference` pipes
    inLoop = dfs id (adj notPipe) (Set.toList candidates)

adj :: Set.Set Pt -> Pt -> [Pt]
adj rng pt = [x | x <- cardinal pt, Set.member x rng]

onLeft :: Map Pt Char -> (Pt, Pt) -> [Pt]
onLeft grid (pos, dir) =
  [turnLeft d + pos | d <- [dir, pipe (grid Map.! pos) dir]]

step :: Map Pt Char -> (Pt, Pt) -> [(Pt, Pt)]
step grid (dir,pos) =
  [(dir', pos + dir') | let dir' = pipe (grid Map.! pos) dir]

pipe :: Char -> Pt -> Pt
pipe = \case
  'S' -> id
  '-' -> id
  '|' -> id
  '7' -> invert
  'L' -> invert
  'F' -> invert'
  'J' -> invert'
  _   -> error "bad pipe"

dfs :: Ord t => (a -> t) -> (a -> [a]) -> [a] -> [a]
dfs rep next = loop Set.empty
  where
    loop !seen = \case
      [] -> []
      x:xs | Set.member r seen -> loop seen xs
           | otherwise -> x : loop seen1 (next x ++ xs)
        where
          r = rep x
          seen1 = Set.insert r seen


findStart :: Map Pt Char -> (Pt,Pt)
findStart grid = head
  [(pos,dir) | (pos,'S') <- Map.assocs grid
             , (dir,valid) <- [(south, "J|L"), (west, "-LF"), (north, "|F7")]
             , let next = Map.findWithDefault '.' (pos+dir) grid
             , next `elem` valid]