#!/usr/bin/env stack
-- stack script --resolver lts-12.21

import Data.Char (isNumber)
import Data.List (minimumBy)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

parseLine :: String -> (Int, Int)
parseLine str =
  let (lhs, rhs) = break (not . isNumber) str
  in (read lhs, read $ tail rhs)

getMaxCoord :: [(Int, Int)] -> (Int, Int)
getMaxCoord = foldl (\(mx, my) (x, y) -> (max mx x, max my y)) (0, 0)

dist :: (Int, Int) -> (Int, Int) -> Int
dist (lx, ly) (rx, ry) = (max lx rx - min lx rx) + (max ly ry - min ly ry)

closest :: [(Int, (Int, Int))] -> (Int, Int) -> Int
closest sourceList target =
  fst $ foldl (\acc@(accId, accDist) (id, coord) ->
    let
      currentDist = dist coord target
    in
      case compare currentDist accDist of
        LT -> (id, currentDist)
        EQ -> (-1, currentDist)
        GT -> acc
  ) (-1, maxBound) sourceList

indexed :: [a] -> [(Int, a)]
indexed list = zip [0..(length list)] list

count :: Int -> Map Int Int -> Map Int Int
count key map =
  case Map.lookup key map of
    Nothing ->
      Map.insert key 1 map
    Just value ->
      Map.insert key (value + 1) map

accountInfinite :: (Int, Int) -> (Int, (Int, Int)) -> Set Int -> Set Int
accountInfinite (xmax, ymax) (owner, (x, y)) set =
  if x <= 0 || y <= 0 || x >= xmax || y >= ymax then
    Set.insert owner set
  else
    set

maximumArea :: [(Int, Int)] -> Int
maximumArea sources =
  let
    maxCoord@(xmax, ymax) = getMaxCoord sources
    coordList = [(i, j) | j <- [0..ymax], i <- [0..xmax]]
    scenario = map (closest $ indexed sources) coordList
    scenarioCount = foldl (flip count) Map.empty scenario
    initSet = Set.singleton (-1)
    infintes = foldl (flip $ accountInfinite maxCoord) initSet $ zip scenario coordList
    finiteScenario = Set.foldl (\map value -> Map.delete value map) scenarioCount infintes
  in
    maximum $ Map.elems finiteScenario

main :: IO ()
main = do
  coords <- (map parseLine . lines) <$> getContents
  putStrLn $ show $ maximumArea coords
