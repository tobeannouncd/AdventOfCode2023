{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module IntervalTree
  ( empty
  , singleton
  , insert
  , overlap
  , fromList
  , TreeD(..)
  , lookupTree
  ) where

data TreeD k a
  = Nil
  | Node
      { tLeft  :: TreeD k a
      , tLo    :: k
      , tVal   :: a
      , tHi    :: k
      , tRight :: TreeD k a }
  deriving (Show,Functor)

empty :: TreeD k a
empty = Nil


singleton :: (k, k) -> a -> TreeD k a
singleton (lo,hi) val = Node Nil lo val hi Nil

insert :: Ord k => (k, k) -> a -> TreeD k a -> TreeD k a
insert i v Nil = singleton i v
insert i@(lo,hi) v t@(Node l lo' _ hi' r)
  | hi < lo' = t {tLeft = insert i v l}
  | lo > hi' = t {tRight = insert i v r}
  | otherwise = error "trees cannot overlap"

overlap :: (Ord a, Num a) => (a, a) -> TreeD a b -> ([(a, a)], [((a, a), b)])
overlap (lo,hi) _ | lo > hi = ([],[])
overlap i Nil = ([i], [])
overlap (lo,hi) (Node l lo' v hi' r)
  | hi < lo' = left
  | lo > hi' = right
  | otherwise = (la ++ ra, (center, v) : lb ++ rb)
  where
    left@(la,lb) = overlap (lo, min hi (lo'-1)) l
    right@(ra,rb) = overlap (max (hi'+1) lo, hi) r
    center = (max lo lo', min hi hi')

fromList :: Ord k => [((k,k),a)] -> TreeD k a
fromList = foldr (uncurry insert) empty

lookupTree :: Ord a1 => a1 -> TreeD a1 a2 -> Maybe a2
lookupTree _ Nil = Nothing
lookupTree x (Node l lo v hi r)
  | x < lo = lookupTree x l
  | x > hi = lookupTree x r
  | otherwise = Just v