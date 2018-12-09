#!/usr/bin/env stack
-- stack script --resolver lts-12.21

import Data.Maybe (fromMaybe)
import qualified Data.List as List

-- NOTE(alvivi): this works because all strings have the same length
hammingDistance :: Eq a => [a] -> [a] -> Int
hammingDistance = (sum .) . zipWith ((fromEnum .) . (/=))

findAtDistance :: Int -> String -> [String] -> Maybe String
findAtDistance dist base list =
  List.find ((== dist) . hammingDistance base) list

findSimilar :: [String] -> Maybe (String, String)
findSimilar [] = Nothing
findSimilar (x:xs) =
  case findAtDistance 1 x xs of
    Nothing -> findSimilar xs
    Just y -> Just (x, y)

commonString :: String -> String -> String
commonString [] _ = ""
commonString _ [] = ""
commonString (x:xs) (y:ys) = if x == y then x : commonString xs ys else commonString xs ys

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ uncurry commonString $ fromMaybe ("", "") $ findSimilar $ lines contents
