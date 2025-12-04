import Data.Char (digitToInt)
import Data.List (maximumBy)
import Data.Ord (Down (..), comparing)

main :: IO ()
main = do
  content <- readFile "in"
  let banks = map parseLine (lines content)
      result = map (sum . largestJoltage 12) banks
  print $ sum result

largestJoltage :: Int -> [Int] -> [Int]
largestJoltage n bank
  | n <= 0 = []
  | otherwise =
      let pairs = zip bank [1 ..]
          len = length pairs
          pairsWitoutLast = take (len - n + 1) pairs
          (maxDigit, pos) = maximumBy (comparing fst <> comparing (Down . snd)) $ reverse pairsWitoutLast
       in maxDigit * 10 ^ (n - 1) : largestJoltage (n - 1) (drop pos bank)

parseLine :: String -> [Int]
parseLine = map digitToInt
