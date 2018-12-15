#!/usr/bin/env stack
-- stack script --resolver lts-12.21

import Data.Array ((!), (//))
import Data.Sequence (Seq)
import qualified Data.Array as Array
import qualified Data.Sequence as Seq

type CurrentIndex = Int
type Length = Int
data State = State Length CurrentIndex (Seq Int) deriving (Show)

first :: State
first =
  State 1 0 $ Seq.singleton 0

moveClockwise :: Length -> Int -> Int -> Int
moveClockwise length base addition =
  mod (base + addition) length

playTurn :: Int -> State -> (Int, State)
playTurn marbleValue state@(State length index seq) =
  if mod marbleValue 23 == 0 then
    let
      deletedIndex = moveClockwise length index (-7)
      deletedValue = Seq.index seq deletedIndex
      updatedSeq = Seq.deleteAt deletedIndex seq
      updatedIndex = mod deletedIndex (length - 1)
    in
      (marbleValue + deletedValue, State (length - 1) updatedIndex updatedSeq)
  else
    let
      insertIndex = moveClockwise length index 2
      updatedSeq = Seq.insertAt insertIndex marbleValue seq
    in
      (0, State (length + 1) insertIndex updatedSeq)

play :: Int -> Int -> Int
play playerCount marbleCount =
  let
    initScoreboard = Array.listArray (0, playerCount - 1) $ repeat 0
  in
    maximum
    $ Array.elems
    $ fst
    $ foldl (\(scoreboard, state) marble ->
        let
          currentPlayer = rem (marble - 1) playerCount
          playerScore = scoreboard ! currentPlayer
          (score, nextState) = playTurn marble state
          updatedScoreboard = scoreboard // [(currentPlayer, playerScore + score)]
        in
          (updatedScoreboard, nextState)
      ) (initScoreboard, first) [1..marbleCount]
