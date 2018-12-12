#!/usr/bin/env stack
-- stack script --resolver lts-12.21

import Data.Maybe (mapMaybe)

data Node = Node [Node] [Int] deriving (Show)

parse :: [Int] -> ([Int], Node)
parse (nodecount : metadataCount : input) =
  let
    (metadataInput, children) = parseMany nodecount input
    metadata = take metadataCount metadataInput
    remInput = drop metadataCount metadataInput
  in
    (remInput, Node children metadata)

parseMany :: Int -> [Int] -> ([Int], [Node])
parseMany 0 input =
  (input, [])
parseMany count input =
  let
    (nextInput, node) = parse input
    (remInput, nodeList) = parseMany (count - 1) nextInput
  in
    (remInput, node : nodeList)

getByIndex :: Int -> [a] -> Maybe a
getByIndex _ [] =
  Nothing
getByIndex n (x:xs) =
  case compare n 0 of
    LT -> Nothing
    EQ -> Just x
    GT -> getByIndex (n - 1) xs

refs :: Node -> [Node]
refs (Node children metadata) =
  mapMaybe (flip getByIndex children . subtract 1) metadata

checksum :: Node -> Int
checksum (Node [] metadata) =
  sum metadata
checksum node =
  sum $ map checksum $ refs node

main :: IO ()
main = do
  contents <- (map read . words) <$> getContents
  putStrLn $ show $ checksum $ snd $ parse contents
