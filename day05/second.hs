#!/usr/bin/env stack
-- stack script --resolver lts-12.21

import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, findIndex)
import Control.Monad (forM_)

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

filterPar :: Char -> String -> String
filterPar char =
  filter (\schar -> not (char == schar || char == toLower schar))

main :: IO ()
main = do
  contents <- trim <$> getContents
  putStrLn $ "Base length: " ++ (show $ length contents)
  forM_ ['a'..'z'] $ \char -> do
    putStrLn $ "--------- " ++ show char
    let shortContents = filterPar char contents
    putStrLn $ "Length: " ++ (show $ length shortContents)
    let reducedContents = reduce shortContents
    putStrLn $ "Reduced Length: " ++ (show $ length reducedContents)
