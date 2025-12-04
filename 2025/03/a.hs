import Data.Char (digitToInt)
import Data.List (maximumBy)
import Data.Ord (comparing, Down (..))

main :: IO ()
main = do
  content <- readFile "in"
  let banks = lines content
      result = map largestJoltage banks
  print $ sum result

largestJoltage :: String -> Int
largestJoltage s =
  let js = map digitToInt s
      pairs = zip js [1 ..]
      pairsWitoutLast = init pairs
      (l, p) = maximumBy (comparing fst <> comparing (Down . snd)) $ reverse pairsWitoutLast
      pairsFromMax = drop p pairs
      (r, _) = maximum pairsFromMax
   in l * 10 + r
