import Control.Monad.ST
import Data.List (elemIndex, findIndex)
import Data.Maybe (fromMaybe)
import Data.STRef
import Data.Set qualified as S
import Data.Map qualified as M

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
      result = beam grid start maxY
   in result

beam :: Grid -> Point -> Int -> Int
beam grid start maxY = runST $ do
  -- mutable reference to keep state of visited splitters
  visited <- newSTRef (M.empty :: M.Map (Int, Int) Int)

  -- recurcive helper to walk the manifold
  let walk (x, y) = do
        visited' <- readSTRef visited
        case findSplitPos grid (x, y) maxY of
          Nothing -> return 1
          Just (x', y') -> do
            if (x', y') `M.member` visited'
              then return $ visited' M.! (x', y')
              else do
                a <- walk (x' - 1, y')
                b <- walk (x' + 1, y')
                modifySTRef' visited (M.insertWith (+) (x', y') (a + b))
                return (a + b)
  -- run and return result
  walk start

findSplitPos :: Grid -> Point -> Int -> Maybe Point
findSplitPos grid (x, y) maxY = do
  yOffset <- findIndex (== True) [(x, y') `S.member` grid | y' <- [y .. maxY]]
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
