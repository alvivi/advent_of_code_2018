#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
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

solve :: Int -> [Int]
solve count =
  let (_, _, state) = stepMany (count + 10) initState
  in toList $ Seq.take 10 $ Seq.drop count state
