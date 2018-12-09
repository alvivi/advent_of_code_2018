#!/usr/bin/env stack
-- stack script --resolver lts-12.21

import Data.List (sort, maximumBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- TOO HIGH 126617

-- Data

data EntryKind
  = Asleep
  | Awake
  | Shift Int
  deriving (Eq, Show)

data Entry = Entry
  { date :: Int
  , kind :: EntryKind
  , time :: Int
  }
  deriving (Eq, Show)

instance Ord Entry where
  compare lhs rhs =
    compare (date lhs * 10000 + time lhs) (date rhs * 10000 + time rhs)

parseEntry :: String -> Entry
parseEntry line =
  let
    year = read $ split 1 5 $ line
    month = read $ split 6 8 $ line
    day = read $ split 9 11 $ line
    hour = read $ split 12 14 $ line
    minutes = read $ split 15 17 $ line
    kind =
      case split 19 24 $ line of
        "falls" -> Asleep
        "wakes" -> Awake
        "Guard" -> Shift $ read $ flip (!!) 0 $ words $ drop 26 $ line
  in
    Entry
    { date = year * 10000 + month * 100 + day
    , time = hour * 60 + minutes
    , kind = kind
    }

split :: Int -> Int -> String -> String
split start end =
  drop start . take end

-- Problem

type GuardIndex = Map Int [(Int, Int)]

insertInterval :: Int -> (Int, Int) -> GuardIndex -> GuardIndex
insertInterval guard interval index =
  case Map.lookup guard index of
    Nothing ->
      Map.insert guard [interval] index
    Just list ->
      Map.insert guard (interval:list) index

toIndex :: [Entry] -> GuardIndex
toIndex =
  let
    reducer (guard, start, index) entry =
      case kind entry of
        Asleep ->
          (guard, Just $ time entry, index)
        Awake ->
          case start of
            Nothing ->
              (guard, Nothing, index)
            Just start ->
              (guard, Nothing, insertInterval guard (start, time entry - 1) index)
        Shift newGuard ->
          case start of
            Nothing ->
              (newGuard, Nothing, index)
            Just start ->
              (newGuard, Nothing, insertInterval guard (start, time entry - 1) index)

    thd (_, _, a) = a
  in
    thd . foldl reducer (0, Nothing, Map.empty)

insertMinutes :: Int -> Map Int Int -> Map Int Int
insertMinutes min index =
  case Map.lookup min index of
    Nothing ->
      Map.insert min 1 index
    Just count ->
      Map.insert min (count + 1) index

sleepiestMinute :: (Int, [(Int, Int)]) -> (Int, Int, Int)
sleepiestMinute (guard, intervals) =
  let
    minIndex = foldl (flip insertMinutes) Map.empty $ concat $  map (uncurry enumFromTo) intervals
    (sleepiestMin, count) = Map.foldlWithKey (\acc@(min, count) newMin newCount ->
        if newCount > count then (newMin, newCount) else acc
      ) (0, 0) minIndex
  in
    (guard, sleepiestMin, count)

-- IO

main :: IO ()
main = do
  contents <- getContents
  let entries = sort $ map parseEntry $ lines contents
  putStrLn
    $ show
    $ (\(a, b, c) -> a * b)
    $ maximumBy (\(_, _, lhs) (_, _, rhs) -> compare lhs rhs)
    $ Map.foldlWithKey (\acc guard intervals -> sleepiestMinute (guard, intervals) : acc) []
    $ toIndex entries
