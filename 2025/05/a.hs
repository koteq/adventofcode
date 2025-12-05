#!/usr/bin/env cabal
{- cabal:
build-depends:
  base,
  split
-}

import Data.List.Split (splitOn)

main :: IO ()
main = do
  content <- readFile "in"
  let [a, b] = splitOn "\n\n" content
      readRange s = map read (splitOn "-" s) :: [Int]
      freshIdRanges = map readRange (lines a)
      availableIds = map read (lines b) :: [Int]
      isFresh x = any inRange freshIdRanges
        where inRange [l, r] = l <= x && x <= r
      fresh = filter isFresh availableIds
      result = length fresh
  print result
