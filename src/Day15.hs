{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day15 (solve) where

import Data.Array (assocs, listArray, (!), (//))
import Data.Char (ord)
import Data.Foldable (foldl', toList)
import Data.Map.Ordered (delete, empty, (|>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read (decimal)

type Output = (Int, Int)

data Op
  = Ins !Text !Int
  | Del !Text
  deriving (Show, Eq)

parseOp :: Text -> Op
parseOp t
  | [a, b] <- T.splitOn "=" t = Ins a (either (error . show) fst $ decimal b)
  | [a, b] <- T.splitOn "-" t,
    T.null b =
      Del a
  | otherwise = error "cannot parse"

solve :: Text -> Output
solve inp = (sum (map hash initSeq), part2 (map parseOp initSeq))
  where
    initSeq = T.splitOn "," $ T.strip inp

hash :: Text -> Int
hash = T.foldl' (\acc x -> (acc + ord x) * 17 `rem` 256) 0

part2 :: [Op] -> Int
part2 ops = foldl' power 0 (assocs end)
  where
    start = listArray (0, 255) (repeat empty)
    end = foldl' f start ops
    f boxes = \case
      Ins lbl val ->
        let i = hash lbl
            box' = boxes ! i |> (lbl, val)
         in boxes // [(i, box')]
      Del lbl ->
        let i = hash lbl
            box' = delete lbl (boxes ! i)
         in boxes // [(i, box')]
    power acc (i, box) = acc + sum [(i + 1) * j * v | (j, v) <- zip [1 ..] (toList box)]