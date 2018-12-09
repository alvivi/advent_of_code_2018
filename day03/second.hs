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
  { ref :: Int
  , origin :: (Int, Int)
  , size :: (Int, Int)
  }
  deriving (Show)

parseRect :: String -> Rect
parseRect line =
  let
    ws = words $ map (\c -> if isNumber c then c else ' ') line
    vs i = read $ ws !! i
  in
    Rect { ref = vs 0, origin = (vs 1, vs 2), size = (vs 3, vs 4) }

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

findUnique :: [Rect] -> Canvas -> Maybe Int
findUnique [] canvas = Nothing
findUnique (r:rs) canvas =
  let
    (x, y) = origin r
    (w, h) = size r
    perms = [(i, j) | i <- [(x + 1)..(x + w)], j <- [(y + 1)..(y + h)]]
  in
    if all (== Just 1) $ map (flip Map.lookup canvas) perms then
      Just $ ref r
    else
      findUnique rs canvas

main :: IO ()
main = do
  contents <- getContents
  let rectList = map parseRect $ lines contents
  putStrLn $ show $ findUnique rectList $ flip drawRectList Map.empty $ rectList
