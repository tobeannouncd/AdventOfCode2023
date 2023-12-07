module Day07 (solve) where

import Data.Text (Text, unpack)
import Control.Arrow ( Arrow((&&&)) )
import qualified Data.Text as T
import Data.List (sortOn, elemIndex, group, sort)
import Utils (readInts)

type Output = (Int,Int)

data Hand = Hand
  { cards :: [Char]
  , bid   :: Int
  } deriving (Show,Eq)

data HandType
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Show,Eq,Ord)

parseHand :: Text -> Hand
parseHand input = Hand (unpack (T.take 5 input)) (last (readInts input))

scoreOn :: Ord a => (Hand -> a) -> [Hand] -> Int
scoreOn cmp = sum . zipWith (*) [1..] . map bid . sortOn cmp

cmp1 :: Hand -> (HandType, [Maybe Int])
cmp1 = (handType &&& map (`elemIndex` vals)) . cards
  where
    vals = ['2'..'9'] ++ "TJQKA"

cmp2 :: Hand -> (HandType, [Maybe Int])
cmp2 = (handType . filter (/= 'J') &&& map (`elemIndex` vals)) . cards
  where
    vals = ['2'..'9'] ++ "TQKA"

orgByVal :: Ord a => [a] -> [Int]
orgByVal = sort . map length . group . sort

handType :: [Char] -> HandType
handType xs = case orgByVal xs of
  [] -> FiveOfAKind
  [_] -> FiveOfAKind
  [1,_] -> FourOfAKind
  [2,_] -> FullHouse
  [1,1,_] -> ThreeOfAKind
  [1,2,_] -> TwoPair
  [_,_,_,_] -> OnePair
  _ -> HighCard

solve :: Text -> Output
solve input = (scoreOn cmp1 &&& scoreOn cmp2) hands
  where
    hands = map parseHand (T.lines input)