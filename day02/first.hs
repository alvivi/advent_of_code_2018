#!/usr/bin/env stack
-- stack script --resolver lts-12.21

import Data.Map (Map)
import qualified Data.Map as Map

-- Histogram

type Histogram a = Map a Int

histogramFromString :: String -> Histogram Char
histogramFromString =
  foldl (flip insertIntoHistogram) Map.empty

insertIntoHistogram :: (Ord a) => a -> Histogram a -> Histogram a
insertIntoHistogram value histogram =
  case Map.lookup value histogram of
    Nothing ->
      Map.insert value 1 histogram
    Just count ->
      Map.insert value (count + 1) histogram

countByQuantity :: Int -> Histogram a -> Int
countByQuantity quantity histogram =
  Map.foldl (\acc value -> if value == quantity then acc + 1 else acc) 0 histogram

-- Problem

checksum :: [String] -> Int
checksum list =
  let
    toCount value = if value >= 1 then 1 else 0

    step (twos, threes) str =
      let
        histogram = histogramFromString str
      in
        ( twos + (toCount $ countByQuantity 2 histogram)
        , threes + (toCount $ countByQuantity 3 histogram)
        )

    (twos, threes) = foldl step (0, 0) list
  in
    twos * threes

-- IO

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ show $ checksum $ lines contents
