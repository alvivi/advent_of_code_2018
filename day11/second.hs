#!/usr/bin/env stack
-- stack script --resolver lts-12.21

{-# LANGUAGE FlexibleContexts #-}

import Data.Array.ST (runSTArray)
import qualified Data.Array.MArray as MArray
import Data.Array.MArray (MArray)
import Data.Array (Array, (!))
import qualified Data.Array as Array
import Control.Monad (forM_)

powerLevelAt :: Int -> Int -> Int -> Int
powerLevelAt serial x y =
  let
    rack = x + 10
    hundreds v = rem (floor (fromIntegral v / 100)) 10
  in
    (hundreds ((rack * y + serial) * rack)) - 5

lineal :: Int -> Int -> Int -> Int
lineal size x y = (y - 1) * size + x

readGrid :: (MArray a Int m) => Int -> Int -> Int -> a Int Int -> m Int
readGrid _ 0 _ _ = return 0
readGrid _ _ 0 _ = return 0
readGrid size x y grid = MArray.readArray grid (lineal size x y)

powerGrid :: Int -> Int -> Array Int Int
powerGrid size serial =
  runSTArray $ do
    grid <- MArray.newArray (1, size * size) 0
    forM_ [1..size] $ \y ->
      forM_ [1..size] $ \x -> do
        left <- readGrid size (x - 1) y grid
        up <- readGrid size x (y - 1) grid
        corner <- readGrid size (x - 1) (y - 1) grid
        let value = powerLevelAt serial x y + left + up - corner
        MArray.writeArray grid (lineal size x y) value
    return grid

powerArea :: Int -> Int -> Int -> Array Int Int -> Int
powerArea size x y grid =
  let
    side = floor $ sqrt $ fromIntegral  $ snd $ Array.bounds grid
  in
    grid ! lineal side (x + size - 1) (y + size - 1)
    - grid ! lineal side (x + size - 1) y
    - grid ! lineal side x (y + size - 1)
    + grid ! lineal side x y

largestWithSize :: Int -> Array Int Int -> (Int, (Int, Int))
largestWithSize size grid =
  let
    side = floor $ sqrt $ fromIntegral $ snd $ Array.bounds grid
    maxCoord = side - size
    coords = [(i, j) | j <- [1 .. maxCoord], i <- [1 .. maxCoord]]
  in
    foldl (\acc@(largestPower, _) (x, y) ->
      let cellPower = powerArea (size + 1) x y grid
      in if cellPower > largestPower then (cellPower, (x + 1, y + 1)) else acc
    ) (minBound, (-1, -1)) coords

largest :: Array Int Int -> (Int, Int, (Int, Int))
largest grid =
  let
    side = floor $ sqrt $ fromIntegral $ snd $ Array.bounds grid
  in
    foldl (\acc@(_, largestPower, _) size ->
      let (cellPower, coord) = largestWithSize size grid
      in if cellPower > largestPower then (size, cellPower, coord) else acc
    ) (minBound, 0, (-1, -1)) [1 .. side]

main :: IO ()
main = do
  serial <- read <$> getContents
  let (size, _, (x, y)) = largest $ powerGrid 300 serial
  putStrLn $ show x ++ "," ++ show y ++ "," ++ show size
