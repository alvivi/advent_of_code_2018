#!/usr/bin/env stack
-- stack script --resolver lts-12.21

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

checksum :: Node -> Int
checksum (Node children metadata) =
  sum metadata + (sum $ map checksum children)

main :: IO ()
main = do
  contents <- (map read . words) <$> getContents
  putStrLn $ show $ checksum $ snd $ parse contents
