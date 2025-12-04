#!/usr/bin/env cabal
{- cabal:
build-depends:
  base,
  array
-}

import Data.Array (Array, array, elems, bounds, (!))

newtype Grid = Grid (Array (Int, Int) Bool)

main :: IO ()
main = do
  content <- readFile "in"
  let grid = parseGrid content
      (Grid accessibleRolls) = filterGrid isAccessibleRoll grid
      cnt = length $ filter (== True) (elems accessibleRolls)
  print cnt

filterGrid :: (Grid -> (Int, Int) -> Bool) -> Grid -> Grid
filterGrid prerdicate (Grid grid) =
  let ((minX, minY), (maxX, maxY)) = bounds grid
   in Grid
        ( array
            ((minX, minY), (maxX, maxY))
            [ ((x, y), prerdicate (Grid grid) (x, y))
              | x <- [minX .. maxX],
                y <- [minY .. maxY]
            ]
        )

isAccessibleRoll :: Grid -> (Int, Int) -> Bool
isAccessibleRoll (Grid grid) (x, y) =
  let isRoll = grid ! (x, y)
      ((minX, minY), (maxX, maxY)) = bounds grid
      inBound (x, y) = minX <= x && x <= maxX && minY <= y && y <= maxY
      adjacentRolls =
        [ inBound (x', y') && grid ! (x', y')
          | (x', y') <- adjacent (x, y)
        ]
      adjacentRollCount = length (filter (== True) adjacentRolls)
   in isRoll && adjacentRollCount < 4

-- helpers

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) =
  [ (x + dx, y + dy)
    | dx <- [-1, 0, 1],
      dy <- [-1, 0, 1],
      (dx, dy) /= (0, 0)
  ]

-- parse

parseGrid :: String -> Grid
parseGrid content =
  let rows = lines content
      width = length (head rows)
      height = length rows
      val (x, y) =
        let row = rows !! y
            char = row !! x
         in char == '@'
   in Grid
        ( array
            ((0, 0), (width - 1, height - 1))
            [ ((x, y), val (x, y))
              | x <- [0 .. width - 1],
                y <- [0 .. height - 1]
            ]
        )

-- show

instance Show Grid where
  show (Grid grid) =
    let ((minX, minY), (maxX, maxY)) = bounds grid
        row y =
          [ if grid ! (x, y) then '@' else '.'
            | x <- [minX .. maxX]
          ]
     in unlines [row y | y <- [minY .. maxY]]
