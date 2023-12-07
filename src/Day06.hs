module Day06 (solve) where

import Data.Text (Text)
import Control.Arrow ( Arrow((&&&)) )
import qualified Data.Text as T
import Utils (readInts)

type Output = (Int,Int)

solve :: Text -> Output
solve = (part1 &&& part2) . parse

part1 :: [(Int, Int)] -> Int
part1 = product . map (uncurry ways)

part2 :: (Integral p, Read p, Show a) => [(a, a)] -> p
part2 games = ways (conc ts) (conc rs)
  where
    (ts, rs) = unzip games
    conc = read . concatMap show

-- When released at t ticks, a distance of d can be reached in a T tick race.
-- d(t) = t * (T - t)
-- d(t) > R
-- t * (T - t) > R
-- t^2 - T*t + R < 0
-- t > (T - sqrt (T*T - 4*R)) / 2
-- t = 1 + floor tRecord
-- interval from t to T-t
-- [1 + floor tR, T - 1 - floor tR]
-- interval size = hi - lo + 1
-- size = T - 1 - floor tR - 1 - floor tR + 1

ways :: (Integral p, Integral a) => a -> p -> a
ways time dist = time - 1 - 2 * floor tRecord
  where
    tRecord = (t - sqrt (t*t - 4*d)) / 2 :: Double
    t = fromIntegral time
    d = fromIntegral dist


parse :: Integral b => Text -> [(b, b)]
parse input
  | [a,b] <- map readInts $ T.lines input
  = zip a b
parse _ = undefined
