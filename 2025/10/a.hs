#!/usr/bin/env cabal
{- cabal:
build-depends:
  base,
  parsec
-}

import Data.Either (fromRight)
import Data.List (findIndex)
import Data.Set qualified as S
import Text.ParserCombinators.Parsec
import Data.Maybe (fromJust)

type Lights = [Bool]

data Machine = Machine
  { lights :: Lights,
    buttons :: [[Int]],
    jolts :: [Int]
  }
  deriving (Show)

main :: IO ()
main = do
  content <- readFile "in"
  print $ fromRight (error "Boom!") $ main' content

main' :: String -> Either ParseError Int
main' input = do
  machines <- parse parseMachines "" input
  let presses = map fewestPresses machines
      presses' = map fromJust presses
  return $ sum presses'

fewestPresses :: Machine -> Maybe Int
fewestPresses machine = do
  idx <- findIndex (S.member (lights machine)) (breadthFirstTraversal machine)
  return $ idx + 1

breadthFirstTraversal :: Machine -> [S.Set Lights]
breadthFirstTraversal machine = go $ S.singleton initialLights
  where
    initialLights = replicate (length $ lights machine) False
    -- convert button wirings to light masks for easier application with xor
    lightMasks = map (toggleLights initialLights) $ buttons machine
    go :: S.Set Lights -> [S.Set Lights]
    go currentLightSets = adjacentLights : go adjacentLights
      where
        adjacentLights :: S.Set Lights
        adjacentLights = S.fromList [zipWith (/=) currentLights mask | mask <- lightMasks, currentLights <- S.elems currentLightSets]

toggleLights :: Lights -> [Int] -> Lights
toggleLights lights buttons = [l /= wired pos | (l, pos) <- zip lights [0 ..]]
  where
    wired pos = pos `elem` buttons

-- parsec

parseMachines :: Parser [Machine]
parseMachines = do
  parseMachine `endBy` char '\n'

parseMachine :: Parser Machine
parseMachine = do
  lights <- parseLights
  char ' '
  numberLists <-
    choice
      [ parseNumbersWithinParens ('(', ')'),
        parseNumbersWithinParens ('{', '}')
      ]
      `sepBy` char ' '
  return Machine {lights, buttons = init numberLists, jolts = last numberLists}

parseLights :: Parser Lights
parseLights = do
  char '['
  xs <- many1 parseLight
  char ']'
  return xs

parseLight :: Parser Bool
parseLight = do
  x <- choice [char '.', char '#']
  return (x == '#')

parseNumbersWithinParens :: (Char, Char) -> Parser [Int]
parseNumbersWithinParens (a, b) = do
  char a
  xs <- many1 digit `sepBy` char ','
  char b
  return $ map read xs
