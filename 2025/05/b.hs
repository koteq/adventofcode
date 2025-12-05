#!/usr/bin/env cabal
{- cabal:
build-depends:
  base,
  split
-}

import Data.List (sort, sortBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

main :: IO ()
main = do
  -- read and parse
  content <- readFile "in"
  let [rangesData, _] = splitOn "\n\n" content
      readRange s =
        let [x, y] = map read (splitOn "-" s) :: [Int]
         in (x, y)
      ranges = map readRange (lines rangesData)
      -- sort for folding in O(n) time
      sortedRanges = sortBy (comparing fst) ranges
      -- fold
      addRange ranges (x2, y2)
        | null ranges = [(x2, y2)]
        | y1 < x2 = ranges ++ [(x2, y2)]
        | otherwise = init ranges ++ [(min x1 x2, max y1 y2)]
        where
          (x1, y1) = last ranges
      foldedRanges = foldl addRange [] sortedRanges
      -- count total
      result = foldl (\acc (x, y) -> acc + (y - x + 1)) 0 foldedRanges

  print result
