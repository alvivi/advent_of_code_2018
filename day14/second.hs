#!/usr/bin/env stack
-- stack script --resolver lts-12.21

{-# LANGUAGE BangPatterns #-}

import qualified Data.Sequence as Seq
import Data.List (tails, isPrefixOf)

list :: [Int]
list =
  let
    step !fstIndex !sndIndex !seq =
      let
        fstValue = Seq.index seq fstIndex
        sndValue = Seq.index seq sndIndex
        sumValue = fstValue + sndValue
        newValues =
          if sumValue <= 9
            then [sumValue]
            else [div sumValue 10, rem sumValue 10]
        newSeq = seq <> Seq.fromList newValues
        newLength = Seq.length seq + length newValues
        newFstIndex = rem (fstIndex + fstValue + 1) newLength
        newSndIndex = rem (sndIndex + sndValue + 1) newLength
      in
        newValues ++ step newFstIndex newSndIndex newSeq
  in
    3 : 7 : step 0 1 (Seq.fromList [3, 7])

find :: [Int] -> Int
find xs = length $ takeWhile (not . (xs `isPrefixOf`)) $ tails list
