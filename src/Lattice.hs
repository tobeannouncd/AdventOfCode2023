{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Lattice where
import GHC.Generics (Generic)
import Data.Data (Data)
import Data.Foldable (Foldable(toList))

data Pt = Pt !Int !Int
  deriving (Show, Eq, Read, Ord, Generic, Data)

boundingBox :: Foldable t => t Pt -> Maybe (Pt, Pt)
boundingBox t =
  case toList t of
    [] -> Nothing
    Pt y x : pts -> go y x y x pts
  where
    go yLo xLo yHi xHi [] = lo `seq` hi `seq` Just (lo,hi)
      where
        lo = Pt yLo xLo
        hi = Pt yHi xHi
    go yLo xLo yHi xHi (Pt y x : pts) =
      go (min yLo y) (min xLo x) (max yHi y) (max xHi x) pts


ptLines :: String -> [(Pt,Char)]
ptLines str = [(Pt y x,val) | (y,row) <- zip [0..] (lines str)
                            , (x,val) <- zip [0..] row]

-- | Swaps the x and y coordinates
invert :: Pt -> Pt
invert (Pt y x) = Pt x y

-- | Swaps and negates the x and y coordinates
invert' :: Pt -> Pt
invert' (Pt y x) = Pt (-x) (-y)

turnLeft :: Pt -> Pt
turnLeft (Pt y x) = Pt (-x) y

turnRight :: Pt -> Pt
turnRight (Pt y x) = Pt x (-y)

turnAround :: Pt -> Pt
turnAround (Pt y x) = Pt (-y) (-x)

origin :: Pt
origin = Pt 0 0

north :: Pt
north = Pt (-1) 0

south :: Pt
south = Pt 1 0

west :: Pt
west = Pt 0 (-1)

east :: Pt
east = Pt 0 1

ptRow :: Pt -> Int
ptRow (Pt row _) = row

ptCol :: Pt -> Int
ptCol (Pt _ col) = col

above :: Pt -> Pt
above (Pt y x) = Pt (y-1) x

below :: Pt -> Pt
below (Pt y x) = Pt (y+1) x

left :: Pt -> Pt
left (Pt y x) = Pt y (x-1)

right :: Pt -> Pt
right (Pt y x) = Pt y (x+1)

cardinal :: Pt -> [Pt]
cardinal pt = [above pt, left pt, right pt, below pt]

adjacent :: Pt -> [Pt]
adjacent pt = [above pt, left pt, right pt, below pt,
               above (left pt), above (right pt),
               below (left pt), below (right pt)]

manhattan :: Pt -> Pt -> Int
manhattan a b = norm1 (a - b)

norm1 :: Pt -> Int
norm1 (Pt y x) = abs y + abs x

mapPt :: (Int -> Int) -> Pt -> Pt
mapPt f (Pt y x) = Pt (f y) (f x)

zipPt :: (Int -> Int -> Int) -> Pt -> Pt -> Pt
zipPt f (Pt y1 x1) (Pt y2 x2) = Pt (f y1 y2) (f x1 x2)

instance Num Pt where
  (+) = zipPt (+)
  {-# INLINE (+) #-}
  (-) = zipPt (-)
  {-# INLINE (-) #-}
  (*) = zipPt (*)
  {-# INLINE (*) #-}
  abs = mapPt abs
  {-# INLINE abs #-}
  negate = mapPt negate
  {-# INLINE negate #-}
  signum = mapPt signum
  {-# INLINE signum #-}
  fromInteger = (\x -> Pt x x) . fromInteger
  {-# INLINE fromInteger #-}