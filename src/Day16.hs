{-# LANGUAGE LambdaCase #-}

module Day16 (solve) where

import Control.Monad.State (execState, gets, modify)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text, unpack)
import Lattice

type Output = (Int, Int)

solve :: Text -> Output
solve inp =
  ( energize grid (Pt 0 0) east,
    maximum $ map (uncurry $ energize grid) starts
  )
  where
    grid = Map.fromList $ ptLines $ unpack inp
    Pt yMax xMax = maximum $ Map.keys grid
    starts :: [(Pt, Pt)]
    starts =
      [(Pt 0 x, south) | x <- [0 .. xMax]]
        ++ [(Pt yMax x, north) | x <- [0 .. xMax]]
        ++ [(Pt y 0, east) | y <- [0 .. yMax]]
        ++ [(Pt y xMax, west) | y <- [0 .. yMax]]

energize :: Map Pt Char -> Pt -> Pt -> Int
energize grid startPos startDir =
  Set.size . Set.fromList . map fst . Set.elems $
    execState (travel [(startPos, startDir)]) Set.empty
  where
    travel [] = pure ()
    travel ((pt, dir) : xs) = case grid Map.!? pt of
      Nothing -> travel xs
      Just c -> do
        visited <- gets (Set.member (pt, dir))
        if visited
          then travel xs
          else do
            modify (Set.insert (pt, dir))
            travel (go pt dir c ++ xs)

    go pt dir = \case
      '.' -> [(pt + dir, dir)]
      '/' -> let d = invert' dir in [(pt + d, d)]
      '\\' -> let d = invert dir in [(pt + d, d)]
      '|'
        | ptCol dir == 0 -> [(pt + dir, dir)]
        | otherwise ->
            let d1 = turnLeft dir
                d2 = turnRight dir
             in [(pt + d1, d1), (pt + d2, d2)]
      '-'
        | ptRow dir == 0 -> [(pt + dir, dir)]
        | otherwise ->
            let d1 = turnLeft dir
                d2 = turnRight dir
             in [(pt + d1, d1), (pt + d2, d2)]
      _ -> error "invalid grid char"