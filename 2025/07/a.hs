import Control.Monad.ST
import Data.List (elemIndex, findIndex)
import Data.Maybe (fromMaybe)
import Data.STRef
import Data.Set qualified as S

type Point = (Int, Int)
type Grid = S.Set (Int, Int)

main = do
  content <- readFile "in"
  print $ main' content

main' input =
  let rows = lines input
      grid = readGrid rows
      start = readStart $ head rows
      maxY = length rows - 1
      visited = beam grid start maxY
      result = S.size visited
   in result

beam :: Grid -> Point -> Int -> S.Set Point
beam grid start maxY = runST $ do
  -- mutable reference to keep state of visited splitters
  visited <- newSTRef S.empty

  -- recurcive helper to walk the manifold
  let walk (x, y) = do
        visited' <- readSTRef visited
        case findSplitPos grid (x, y) maxY of
          Nothing -> return ()
          Just (x', y') -> do
            if (x', y') `S.member` visited'
            then return ()
            else do
              modifySTRef' visited (S.insert (x', y'))
              walk (x' - 1, y')
              walk (x' + 1, y')

  -- run and return result
  walk start
  readSTRef visited

findSplitPos :: Grid -> Point -> Int -> Maybe Point
findSplitPos grid (x, y) maxY = do
  yOffset <- findIndex (==True) [(x, y') `S.member` grid | y' <- [y .. maxY]]
  return (x, y + yOffset)

-- parsing

readGrid rows =
  S.fromList
    [ (x, y)
      | x <- [0 .. maxX],
        y <- [0 .. maxY],
        rows !! y !! x == '^'
    ]
  where
    maxY = length rows - 1
    maxX = length (head rows) - 1

readStart row = (x, y)
  where
    x = fromMaybe (-1) $ elemIndex 'S' row
    y = 0
