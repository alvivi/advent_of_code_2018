#!/usr/bin/env stack
-- stack script --resolver lts-12.21

{-# LANGUAGE NamedFieldPuns #-}

import Data.Char (isNumber)
import Data.Map (Map)
import qualified Data.Map as Map

type Canvas = Map (Int, Int) Int

draw :: (Int, Int) -> Canvas -> Canvas
draw coord canvas =
  case Map.lookup coord canvas of
    Nothing ->
      Map.insert coord 1 canvas
    Just value ->
      Map.insert coord (value + 1) canvas

data Rect = Rect
  { origin :: (Int, Int)
  , size :: (Int, Int)
  }
  deriving (Show)

parseRect :: String -> Rect
parseRect line =
  let
    ws = words $ map (\c -> if isNumber c then c else ' ') line
    vs i = read $ ws !! i
  in
    Rect { origin = (vs 1, vs 2), size = (vs 3, vs 4) }

drawRect :: Rect -> Canvas -> Canvas
drawRect (Rect {origin, size}) canvas =
  let
    (x, y) = origin
    (w, h) = size
    perms = [(i, j) | i <- [(x + 1)..(x + w)], j <- [(y + 1)..(y + h)]]
  in
    foldl (flip draw) canvas perms

drawRectList :: [Rect] -> Canvas -> Canvas
drawRectList list canvas =
  foldl (flip drawRect) canvas list

count :: Canvas -> Int
count =
  length . filter (>= 2) . map snd . Map.toList

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ show $ count $ flip drawRectList Map.empty $ map parseRect $ lines contents
