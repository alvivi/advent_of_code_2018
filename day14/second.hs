#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE Strict #-}

import qualified Data.Foldable as Foldable
import Data.Sequence (Seq, ViewR((:>), EmptyR), (|>))
import qualified Data.Sequence as Seq

initState :: (Int, Int, Seq Int)
initState = (0, 1, Seq.fromList [3, 7])

appendMany :: [a] -> Seq a -> Seq a
appendMany [] seq = seq
appendMany (v:vs) seq = appendMany vs $ seq |> v

step :: (Int, Int, Seq Int) -> (Int, Int, Seq Int)
step (fstIndex, sndIndex, seq) =
  let fstValue = Seq.index seq fstIndex
      sndValue = Seq.index seq sndIndex
      sumValue = fstValue + sndValue
      newValues =
        if sumValue <= 9
          then [sumValue]
          else [div sumValue 10, rem sumValue 10]
      newLength = Seq.length seq + length newValues
      newFstIndex = rem (fstIndex + fstValue + 1) newLength
      newSndIndex = rem (sndIndex + sndValue + 1) newLength
  in (newFstIndex, newSndIndex, appendMany newValues seq)

stepMany :: Int -> (Int, Int, Seq Int) -> (Int, Int, Seq Int)
stepMany 0 state = state
stepMany n state = stepMany (n - 1) $ step state

takeRV :: Int -> ViewR a -> [a]
takeRV 0 _ = []
takeRV n (xs :> x) = x : takeRV (n - 1) (Seq.viewr xs)
takeRV n EmptyR = []

takeR :: Int -> Seq a -> [a]
takeR n seq = reverse $ takeRV n $ Seq.viewr seq

check :: [Int] -> (Int, Int, Seq Int) -> Bool
check pattern (_, _, seq) = pattern == takeR (length pattern) seq

solveStep :: Int -> [Int] -> (Int, Int, Seq Int) -> Int
solveStep count pattern state@(_, _, seq) =
  if check pattern state
    then Seq.length seq - length pattern
    else solveStep (count + 1) pattern (step state)

solve :: [Int] -> Int
solve pattern = (solveStep 0 pattern initState)

main :: IO ()
main = print $ solve [6, 5, 2, 6, 0, 1]
