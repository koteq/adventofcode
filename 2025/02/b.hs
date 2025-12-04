{- cabal:
build-depends:
  base,
  split
-}

import Data.List.Split (chunksOf, splitOn)

main :: IO ()
main = do
  content <- readFile "in"
  let ranges = parseRanges content
      invalidIds = map findInvalidIds ranges
      result = sum $ map sum invalidIds
  print result

findInvalidIds :: (Int, Int) -> [Int]
findInvalidIds (a, b)
  | n <= b = n : findInvalidIds (n + 1, b)
  | otherwise = []
  where
    n = nextInvalidId (a - 1)

nextInvalidId :: Int -> Int
nextInvalidId x =
  let len = length (show x)
      len' = length (show (x + 1))
      -- I've got bored
      rep = [nextRepeated chunkSize x | chunkSize <- [1 .. len `div` 2], len `mod` chunkSize == 0]
      rep' = [nextRepeated chunkSize (x + 1) | chunkSize <- [1 .. len' `div` 2], len' `mod` chunkSize == 0]
      rep'' = [replicateId (len + 1) 1]
      rep''' = [replicateId (len `div` chunkSize) (head (chunksOfId chunkSize x) + 1) | chunkSize <- [1 .. len `div` 2], len `mod` chunkSize == 0]
      rep'''' = [replicateId (len `div` chunkSize - 1) (head (chunksOfId chunkSize x) + 1) | chunkSize <- [1 .. len `div` 2], len `mod` chunkSize == 0]
      results = filter (> x) (rep ++ rep' ++ rep'' ++ rep''' ++ rep'''')
   in minimum results

nextRepeated :: Int -> Int -> Int
nextRepeated chunkSize x =
  let len = length (show x)
      chunks = chunksOfId chunkSize x
      rep = foldr1 (\x acc -> if x < acc then x + 1 else x) chunks
   in replicateId (len `div` chunkSize) rep

-- helpers

chunksOfId :: Int -> Int -> [Int]
chunksOfId n x =
  let s = show x
      c = chunksOf n s
   in map read c

replicateId :: Int -> Int -> Int
replicateId n x =
  let len = length (show x)
   in sum [x * 10 ^ (len * i) | i <- [0 .. n - 1]]

-- parsing

parseRanges :: String -> [(Int, Int)]
parseRanges = map toRange . splitOn ","
  where
    toRange s =
      let [a, b] = splitOn "-" s
       in (read a, read b)
