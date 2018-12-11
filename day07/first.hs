#!/usr/bin/env stack
-- stack script --resolver lts-12.21

import Data.List (sort)
import qualified Data.Set as Set
import Debug.Trace (trace)

parse :: String -> (Char, Char)
parse line = (head $ drop 5 line, head $ drop 36 line)

roots :: [(Char, Char)] -> [Char]
roots rels =
  let
    targets = Set.fromList $ map fst rels
    sources = Set.fromList $ map snd rels
  in
    sort $ Set.toList $ Set.difference targets sources

apply :: Char -> [(Char, Char)] -> ([Char], [(Char, Char)])
apply source rels =
  let (from, otherRels) = splitBy ((== source) . fst) rels
  in (map snd from, otherRels)

splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy pred =
  let step (lhs, rhs) x = if pred x then (x:lhs, rhs) else (lhs, x:rhs)
  in foldl step ([], [])

walk :: [(Char, Char)] -> [Char]
walk [] = []
walk rels =
  case roots rels of
    [] -> []
    (src:_) ->
      case apply src rels of
        ([], _) -> []
        (more, []) -> src : sort more
        (_, nextRels) -> src : walk nextRels

main :: IO ()
main = do
  rels <- (map parse . lines) <$> getContents
  putStrLn $ walk rels
