#!/usr/bin/env stack
-- stack script --resolver lts-12.21

import Data.List (sort)
import Data.Map (Map)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set

type State = [(Int, Char)]

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
  let
    (from, otherRels) = splitBy ((== source) . fst) rels
    applied = map snd from
    ready = filter (\task -> all ((/= task) . snd) otherRels) applied
  in
    (ready, otherRels)

applyMany :: [Char] -> [(Char, Char)] -> ([Char], [(Char, Char)])
applyMany sourceList rels =
  foldl (\(accTargets, accRels) source ->
    let (targets, nextRels) = apply source accRels
    in (targets ++ accTargets, nextRels)
  ) ([], rels) sourceList

splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy pred =
  let step x (lhs, rhs) = if pred x then (x:lhs, rhs) else (lhs, x:rhs)
  in foldr step ([], [])

addTask :: Int -> Char -> State -> State
addTask cost task state =
  let
    taskCost = cost + Char.ord task - ((Char.ord 'A') - 1)
  in
    if any ((==) task . snd) state then
      state
    else
      state ++ [(taskCost, task)]

addTaskMany :: Int -> [Char] -> State -> State
addTaskMany cost taskList state =
  foldl (flip $ addTask cost) state taskList

workStep :: Int -> State -> State
workStep workerCount state =
  let
    step (0, acc) task = (0, task : acc)
    step (count, acc) task@(0, _) = (count, task : acc)
    step (count, acc) task@(todo, id) = (count - 1, (todo - 1, id) : acc)
  in
    reverse $ snd $ foldl step (workerCount, []) state

extractFinished :: State -> ([Char], State)
extractFinished state =
  let
    (finished, pending) = splitBy ((<= 0) . fst) state
  in
    (map snd finished, pending)

doWalk :: Int -> Int -> State -> [(Char, Char)] -> Int
doWalk baseCost workerCount state [] =
  if null state then
    0
  else
    let (_, nextState) = extractFinished state
    in if null nextState then
      0
    else
      1 + doWalk baseCost workerCount (workStep workerCount nextState) []
doWalk baseCost workerCount state rels =
  case extractFinished state of
    ([], cleanState) ->
      1 + doWalk baseCost workerCount (workStep workerCount cleanState) rels
    (finished, cleanState) ->
      let
         (readyTasks, nextRels) = applyMany finished rels
         nextState = addTaskMany baseCost readyTasks cleanState
      in
        1 + doWalk baseCost workerCount (workStep workerCount nextState) nextRels

walk :: Int -> Int -> [(Char, Char)] -> Int
walk baseCost workerCount rels =
  let initState = addTaskMany baseCost (roots rels) []
  in doWalk baseCost workerCount initState rels

main :: IO ()
main = do
  rels <- (map parse . lines) <$> getContents
  putStrLn $ show $ walk 60 5 rels
