#!/usr/bin/env stack
-- stack script --resolver lts-12.21

import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, findIndex)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

react :: Char -> Char -> Bool
react lhs rhs = lhs /= rhs && toLower lhs == toLower rhs

pair :: [a] -> [(a, a)]
pair list = zip list $ tail list

dropTwoAtIndex :: Int -> [a] -> [a]
dropTwoAtIndex index list = take index list ++ drop (index + 2) list

reduce :: String -> String
reduce str =
  case findIndex (uncurry react) $ pair str of
    Nothing -> str
    Just index -> reduce $ dropTwoAtIndex index str

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ show $ length $ reduce $ trim contents
