#!/usr/bin/env stack
-- stack script --resolver lts-12.21

{-# LANGUAGE Strict #-}

import Data.Map (Map)
import Text.ParserCombinators.ReadP (ReadP)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Text.ParserCombinators.ReadP as P

-- Problem

type Bag k v = Map k [v]

bagInsert :: Ord k => k -> v -> Bag k v -> Bag k v
bagInsert key value bag =
  case Map.lookup key bag of
    Nothing ->
      Map.insert key [value] bag
    Just list ->
      Map.insert key (value:list) bag

bagFromList :: Ord k => [(k, v)] -> Bag k v
bagFromList =
  foldl (flip $ uncurry bagInsert) Map.empty

step :: Bag (Int, Int) (Int, Int) -> Bag (Int, Int) (Int, Int)
step =
  let
    insert (x, y) v@(vx, vy) = bagInsert (x + vx, y + vy) v
    insertMany bag p vs = foldl (\acc v -> insert p v acc) bag vs
  in
    Map.foldlWithKey insertMany Map.empty

heuristic :: Bag (Int, Int) (Int, Int) -> Int
heuristic bag =
  let
    ((xMin, yMin), (xMax, yMax)) = boundingBox bag
    exists = Maybe.isJust . flip Map.lookup bag
    hr ox oy = length $ takeWhile (\x -> exists (x, oy)) [ox..xMax]
    hl ox oy = length $ takeWhile (\x -> exists (x, oy)) [xMin..ox]
    vt ox oy = length $ takeWhile (\y -> exists (ox, y)) [yMin..oy]
    vb ox oy = length $ takeWhile (\y -> exists (ox, y)) [oy..yMax]
    compute (x, y) = hr x y + hl x y + vt x y + vb x y
  in
    foldl (\acc p -> acc + compute p) 0 $ Map.keys bag

find :: Bag (Int, Int) (Int, Int) -> Bag (Int, Int) (Int, Int)
find bag =
  let
    findStep 0 _ bestState _ = bestState
    findStep fails bestScore bestState state =
      let
        score = heuristic state
      in
        if score < bestScore then
          findStep (fails - 1) bestScore bestState $ step state
        else
          findStep fails score state $ step state
  in
    findStep 100 (heuristic bag) bag (step bag)

boundingBox :: Bag (Int, Int) (Int, Int) -> ((Int, Int), (Int, Int))
boundingBox =
  let
    init = ((maxBound, maxBound), (minBound, minBound))
    compute ((xMin, yMin), (xMax, yMax)) (x, y) =
      ((min xMin x, min yMin y), (max xMax x, max yMax y))
  in
    foldl compute init . Map.keys


bagShow :: Bag (Int, Int) (Int, Int) -> String
bagShow bag =
  let
    ((xMin, yMin), (xMax, yMax)) = boundingBox bag
    char position =
      case Map.lookup position bag of
        Nothing -> '.'
        Just _ -> '#'
  in
    unlines $ map (\y -> map (\x -> char (x, y)) [xMin..xMax]) [yMin..yMax]

-- Parsing

number :: ReadP Int
number =
  do
    sign <- P.option '+' P.get
    digits <- P.munch1 Char.isDigit
    return $ (if sign == '-' then (-1) else 1) * read digits

vector :: ReadP (Int, Int)
vector =
  do
    P.skipSpaces
    x <- number
    P.char ','
    P.skipSpaces
    y <- number
    return (x, y)

point :: ReadP ((Int, Int), (Int, Int))
point =
  let
    wrappedVector = P.between (P.char '<') (P.char '>') vector
  in do
    P.string "position="
    position <- wrappedVector
    P.skipSpaces
    P.string "velocity="
    velocity <- wrappedVector
    P.skipSpaces
    return (position, velocity)

parse :: String -> Bag (Int, Int) (Int, Int)
parse =
  let
    read = P.readP_to_S point
  in
    bagFromList . map (fst . last . read) . lines



main :: IO ()
main = do
  state <- parse <$> getContents
  putStrLn $ bagShow $ find state
