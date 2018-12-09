#!/usr/bin/env stack
-- stack script --resolver lts-12.21

import Data.List (delete)

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ show $ sum $ map (read . delete '+') $ lines contents
