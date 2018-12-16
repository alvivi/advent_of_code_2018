#!/usr/bin/env stack
-- stack script --resolver lts-12.21

import Data.Set (Set)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Control.Monad
import qualified Data.IORef as Ref

type Rule = ([Bool], Bool)

parse :: String -> ([Rule], Set Int)
parse input =
  let
    lineList = lines input
    initialStateString = drop 2 $ dropWhile (/= ':') $ lineList !! 0
    initialState =
      foldl (\acc (chr, idx) ->
        if chr == '#' then Set.insert idx acc else acc
      ) Set.empty $ zip initialStateString [0..]
    ruleStringList = drop 2 lineList
    stringToRule string =
      ( map (== '#') $ take 5 $ string
      , (== '#') $ head $ drop 9 $ string
      )
  in
    (map stringToRule ruleStringList, initialState)

applyRule :: Set Int -> Int -> Rule -> Maybe Bool
applyRule state idx ([lf, lc, c, rc, rf], r) =
  if lf == Set.member (idx - 2) state
     && lc == Set.member (idx - 1) state
     && c == Set.member idx state
     && rc == Set.member (idx + 1) state
     && rf == Set.member (idx + 2) state
  then
    Just r
  else
    Nothing

step :: [Rule] -> Set Int -> Set Int
step ruleList state =
  let
    Just (minPot, _) = Set.minView state
    Just (maxPot, _) = Set.maxView state
    applyRuleList idx =
      Maybe.listToMaybe $ Maybe.catMaybes $ map (applyRule state idx) ruleList
  in
    foldl (\acc idx ->
      case applyRuleList idx of
        Just True -> Set.insert idx acc
        _ -> acc
    ) Set.empty [minPot - 2 .. maxPot + 2]

stepCount :: Int -> [Rule] -> Set Int -> Set Int
stepCount 0 _ state = state
stepCount count rules state =
  stepCount (count - 1) rules (step rules state)

main :: IO ()
main = do
  (rules, initState) <- parse <$> getContents
  stateRef <- Ref.newIORef initState
  countRef <- Ref.newIORef (sum initState)
  putStrLn $ "0: " ++ show (sum initState) ++ " " ++ show (sum initState)
  forM_ [1..200] $ \i -> do
    prevState <- Ref.readIORef stateRef
    prevCount <- Ref.readIORef countRef
    let state = step rules prevState
    let count = sum state
    Ref.writeIORef stateRef state
    Ref.writeIORef countRef count
    putStrLn $ show i ++ ": " ++ show (count) ++ " " ++ show (count - prevCount)
