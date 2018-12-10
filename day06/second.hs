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

weight :: [(Int, Int)] -> (Int, Int) -> Int
weight sources coord = sum $ map (dist coord) sources

safeArea :: Int -> [(Int, Int)] -> Int
safeArea limit sources =
  let
    maxCoord@(xmax, ymax) = getMaxCoord sources
    coordList = [(i, j) | j <- [0..ymax], i <- [0..xmax]]
  in
    sum $ map (fromEnum . (< limit) . weight sources) coordList

main :: IO ()
main = do
  coords <- (map parseLine . lines) <$> getContents
  putStrLn $ show $ safeArea 10000 coords
