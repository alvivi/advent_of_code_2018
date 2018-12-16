#!/usr/bin/env stack
-- stack script --resolver lts-12.21

import Data.Array (Array, (!))
import qualified Data.Array as Array

powerLevelAt :: Int -> (Int, Int) -> Int
powerLevelAt serial (x, y) =
  let
    rack = x + 10
    hundreds v = rem (floor (fromIntegral v / 100)) 10
  in
    (hundreds ((rack * y + serial) * rack)) - 5

grid :: Int -> Array (Int, Int) Int
grid serial =
  let
    coords = [(i, j) | j <- [1 .. 300], i <- [1 .. 300]]
    toAssoc p = (p, powerLevelAt serial p)
  in
    Array.array ((1,1), (300, 300)) $ map toAssoc coords

power :: (Int, Int) -> Array (Int, Int) Int -> Int
power (x, y) grid =
  grid ! (x, y)
  + grid ! (x, y + 1)
  + grid ! (x, y + 2)
  + grid ! (x + 1, y)
  + grid ! (x + 1, y + 1)
  + grid ! (x + 1, y + 2)
  + grid ! (x + 2, y)
  + grid ! (x + 2, y + 1)
  + grid ! (x + 2, y + 2)

largest :: Array (Int, Int) Int -> (Int, (Int, Int))
largest grid =
  let
    coordList = [(i, j) | j <- [1 .. 298], i <- [1 .. 298]]
  in
    foldl (\acc@(largestPower, _) coord ->
      let
        cellPower = power coord grid
      in
        if cellPower > largestPower then (cellPower, coord) else acc
    ) (minBound, (-1, -1)) coordList
