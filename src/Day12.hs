{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day12 (solve) where

import Data.Text (Text, unpack)
import qualified Data.Text as T
import Utils (readInts)
import Data.List (intercalate)
import Data.Array ( Ix(range), (!), listArray )

type Output = (Int,Int)

solve :: Text -> Output
solve inp = (part1, part2)
  where
    inputs = map parseLine (T.lines inp)
    part1 = sum [ways g s | (s,g) <- inputs]
    part2 = sum [ways (concat $ replicate 5 g) (unfoldSprings s) | (s,g) <- inputs]

ways :: [Int] -> [Char] -> Int
ways groups springs = answer ! (0,0)
  where
    nGroups = length groups
    groupArr = listArray (0, nGroups - 1) groups

    nSprings = length springs
    springArr = listArray (0, nSprings-1) springs

    answerR = ((0,0),(nGroups,nSprings))
    answer = listArray answerR [go i j | (i,j) <- range answerR]

    go iGroup iSpring
      | iGroup == nGroups = fromEnum $
          all (\i -> springArr ! i `elem` ".?") [iSpring .. nSprings - 1]
      | iSpring == nSprings = 0
      | otherwise =
          case springArr ! iSpring of
            '.' -> answer ! (iGroup, iSpring + 1)
            '#' -> checkGroup (iGroup + 1) ((groupArr ! iGroup) - 1) (iSpring + 1)
            '?' -> checkGroup (iGroup + 1) ((groupArr ! iGroup) - 1) (iSpring + 1)
                 + answer ! (iGroup, iSpring + 1)
            _   -> error "invalid character"

    checkGroup iGroup n iSpring
      | iSpring == nSprings = fromEnum $ n == 0 && iGroup == nGroups
      | n == 0, springArr ! iSpring `elem` ".?" = answer ! (iGroup, iSpring + 1)
      | n == 0 = 0
      | '.' == springArr ! iSpring = 0
      | otherwise = checkGroup iGroup (n-1) (iSpring + 1)

unfoldSprings :: [Char] -> [Char]
unfoldSprings = intercalate "?" . replicate 5

parseLine :: Text -> (String, [Int])
parseLine xs =
  let [a,b] = T.words xs
      ns = readInts b
  in (unpack a, ns)