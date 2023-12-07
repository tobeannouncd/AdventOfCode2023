module Utils
  ( readInts,
    paragraphs )
  where

import qualified Data.Text as T
import Data.Text.Read (decimal)

readInts :: Integral a => T.Text -> [a]
readInts t
  | T.null t = []
  | otherwise = case decimal t of
      Left _        -> readInts (T.tail t)
      Right (n, t') -> n : readInts t'

paragraphs :: T.Text -> [T.Text]
paragraphs = T.splitOn (T.pack "\n\n")