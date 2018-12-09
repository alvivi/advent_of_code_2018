#!/usr/bin/env stack
-- stack script --resolver lts-12.21

import Data.List (delete)
import qualified Data.Set as Set

findDup :: [Int] -> Int
findDup =
  let findDup' set (x:xs) = if Set.member x set then x else findDup' (Set.insert x set) xs
  in findDup' Set.empty

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ show $ findDup $ scanl (+) 0 $ cycle $  map (read . delete '+') $ lines contents
