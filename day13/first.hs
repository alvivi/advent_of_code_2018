#!/usr/bin/env stack
-- stack script --resolver lts-12.21

{-# LANGUAGE NamedFieldPuns #-}

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Point = (Int, Int)
data Direction = DTop | DRight | DBottom | DLeft deriving (Eq, Ord)
type DirectionSet = Set (Direction, Direction)
data Scenario =
  Scenario
    { carts :: [(Point, Direction)]
    , vertices :: Map Point DirectionSet
    , rails :: Map Point DirectionSet
    }
  deriving (Show)


instance Show Direction where
  show DTop = "^"
  show DRight = ">"
  show DBottom = "v"
  show DLeft = "<"

example = "\
\/->-\\        \n\
\|   |  /----\\\n\
\| /-+--+-\\  |\n\
\| | |  | v  |\n\
\\\-+-/  \\-+--/\n\
\  \\------/   "

-- Partial Utilities --

vertexCross :: DirectionSet
vertexCross = Set.fromList [(DTop, DBottom), (DBottom, DTop), (DLeft, DRight), (DRight, DLeft)]

vertexLeft :: DirectionSet
vertexLeft = Set.fromList [(DLeft, DBottom), (DBottom, DLeft), (DTop, DRight), (DRight, DTop)]

vertexRight :: DirectionSet
vertexRight = Set.fromList [(DBottom, DRight), (DRight, DBottom), (DTop, DLeft), (DLeft, DTop)]

railVertical :: DirectionSet
railVertical = Set.fromList [(DBottom, DTop), (DTop, DBottom)]

railHorizontal :: DirectionSet
railHorizontal = Set.fromList [(DLeft, DRight), (DRight, DLeft)]

partialEmpty :: Scenario
partialEmpty = Scenario { carts = [] , vertices = Map.empty , rails = Map.empty }

partialAddVertex :: (Int, Int) -> DirectionSet -> Scenario -> Scenario
partialAddVertex point set scenario@(Scenario {vertices}) =
  scenario { vertices = Map.insert point set vertices }

partialAddCross :: (Int, Int) -> Scenario -> Scenario
partialAddCross = flip partialAddVertex vertexCross

partialAddLVertex :: (Int, Int) -> Scenario -> Scenario
partialAddLVertex = flip partialAddVertex vertexLeft

partialAddRVertex :: (Int, Int) -> Scenario -> Scenario
partialAddRVertex = flip partialAddVertex vertexRight

partialAddCart :: (Int, Int) -> Direction -> Scenario -> Scenario
partialAddCart point DTop scenario@(Scenario {carts, rails}) =
  scenario { carts = (point, DTop):carts, rails = Map.insert point railVertical rails }
partialAddCart point DRight scenario@(Scenario {carts, rails}) =
  scenario { carts = (point, DRight):carts, rails = Map.insert point railHorizontal rails }
partialAddCart point DBottom scenario@(Scenario {carts, rails}) =
  scenario { carts = (point, DBottom):carts, rails = Map.insert point railVertical rails }
partialAddCart point DLeft scenario@(Scenario {carts, rails}) =
  scenario { carts = (point, DLeft):carts, rails = Map.insert point railHorizontal rails }

partialAddRail :: (Int, Int) -> DirectionSet -> Scenario -> Scenario
partialAddRail point set scenario@(Scenario {rails}) =
  scenario { rails = Map.insert point set rails }

partialAddHRail :: (Int, Int) -> Scenario -> Scenario
partialAddHRail = flip partialAddRail railHorizontal

partialAddVRail :: (Int, Int) -> Scenario -> Scenario
partialAddVRail = flip partialAddRail railVertical

-- Partial Parsing --

parseStep :: Char -> ((Int, Int), Scenario) -> ((Int, Int), Scenario)
parseStep '\n' ((_, y), partial) = ((0, y + 1), partial)
parseStep ' ' ((x, y), partial) = ((x + 1, y), partial)
parseStep '/' (p@(x, y), partial) = ((x + 1, y), partialAddRVertex p partial)
parseStep '\\' (p@(x, y), partial) = ((x + 1, y), partialAddLVertex p partial)
parseStep '+' (p@(x, y), partial) = ((x + 1, y), partialAddCross p partial)
parseStep '-' (p@(x, y), partial) = ((x + 1, y), partialAddHRail p partial)
parseStep '|' (p@(x, y), partial) = ((x + 1, y), partialAddVRail p partial)
parseStep '^' (p@(x, y), partial) = ((x + 1, y), partialAddCart p DTop partial)
parseStep 'v' (p@(x, y), partial) = ((x + 1, y), partialAddCart p DBottom partial)
parseStep '<' (p@(x, y), partial) = ((x + 1, y), partialAddCart p DLeft partial)
parseStep '>' (p@(x, y), partial) = ((x + 1, y), partialAddCart p DRight partial)

parse :: String -> Scenario
parse = snd . foldl (flip parseStep) ((0, 0), partialEmpty)

main :: IO ()
main = do
  scenario <- parse <$> getContents
  putStrLn $ show scenario
