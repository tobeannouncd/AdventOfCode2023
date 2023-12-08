module Day08 (solve) where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Utils (paragraphs)
import TextParsing
    ( Parser, char, (<|>), string, many1, parse, anyChar )
import Data.Either (fromRight)
import qualified Data.Text as T
import Data.List (elemIndex, unfoldr, findIndex)
import Data.Maybe (fromJust)
import Control.Monad (replicateM)

type Output = (Int,Int)

data Direction = L | R deriving (Show,Eq,Ord,Enum)
type Guide = Map String (String,String)

dirP :: Parser Direction
dirP = L <$ char 'L' <|> R <$ char 'R'

parseInp :: Text -> ([Direction], Guide)
parseInp xs
  | [dirs,guide] <- paragraphs xs
  = ( fromRight undefined $ parse (many1 dirP) "" dirs,
      foldr (uncurry M.insert . parseLoc) M.empty (T.lines guide) )
parseInp _ = undefined

parseLoc :: Text -> (String, (String, String))
parseLoc line = fromRight undefined $ parse p "" line
  where
    p = do
      k <- loc <* string " = ("
      l <- loc <* string ", "
      r <- loc
      return (k, (l, r))
    loc = replicateM 3 anyChar

solve :: Text -> Output
solve inp = (part1 dirs guide, part2 dirs guide)
  where
    (dirs, guide) = parseInp inp

part1 :: [Direction] -> Guide -> Int
part1 dirs guide = pathLenBy (elemIndex "ZZZ") dirs guide "AAA"


part2 :: [Direction] -> Guide -> Int
part2 dirs guide = foldr1 lcm $ map (pathLenBy p dirs guide) starts
  where
    starts = filter ((=='A').last) $ M.keys guide
    p = findIndex ((=='Z').last)

pathLenBy :: ([String] -> Maybe Int) -> [Direction] -> Guide -> String -> Int
pathLenBy p dirs guide start = fromJust . p $ unfoldr move (start, cycle dirs)
  where
    move (pos, d:ds) = Just (pos, (nxt, ds))
      where
        nxt = case guide M.! pos of
          (l,r) -> if d == L then l else r
    move _ = Nothing
