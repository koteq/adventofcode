{- cabal:
build-depends:
  base,
  split
-}

import Data.List.Split (splitOn)

main :: IO ()
main = do
  content <- readFile "in"
  let ranges = parseRanges content
      invalidIds = map findInvalidIds ranges
      result = sum $ map sum invalidIds
  print result

findInvalidIds :: (Int, Int) -> [Int]
findInvalidIds (a, b)
  | nextInvalid <= b = nextInvalid : findInvalidIds (nextInvalid + 1, b)
  | otherwise = []
  where
    nextInvalid = succInvalid (a - 1)

parseRanges :: String -> [(Int, Int)]
parseRanges = map toRange . splitOn ","
  where
    toRange s =
      let [a, b] = splitOn "-" s
       in (read a, read b)

splitIdAt :: Int -> Int -> (Int, Int)
splitIdAt k x = (x `div` p, x `mod` p)
  where
    p = 10 ^ k

combineId :: (Int, Int) -> Int
combineId (a, b) = a * p + b
  where
    p = 10 ^ length (show a)

succInvalid :: Int -> Int
succInvalid x
  | even len = combineId (a', a')
  | otherwise = combineId (a'', a'')
  where
    len = length (show x)
    (a, b) = splitIdAt (len `div` 2) x
    a' = if a > b then a else a + 1
    a'' = 10 ^ (len `div` 2)
