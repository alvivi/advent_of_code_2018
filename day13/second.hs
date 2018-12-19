#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE NamedFieldPuns #-}

import Data.List ((\\), sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Point = (Int, Int)

data Direction
  = DTop
  | DRight
  | DBottom
  | DLeft
  deriving (Eq, Ord)

type DirectionSet = Set (Direction, Direction)

data Scenario = Scenario
  { carts :: [(Point, Direction, [Direction])]
  , crosses :: Set Point
  , rails :: Map Point DirectionSet
  }

instance Show Direction where
  show DTop = "^"
  show DRight = ">"
  show DBottom = "v"
  show DLeft = "<"

instance Show Scenario where
  show Scenario {carts, crosses} =
    let cartsStr = show $ map (\(a, b, _) -> (a, b)) carts
        crossesStr = show $ Set.toList crosses
    in "Scenario{carts=" ++ cartsStr ++ ", " ++ "crosses=" ++ crossesStr ++ "}"

{-
   Partial Utilities
-}
-- '\'
vertexLeft :: DirectionSet
vertexLeft =
  Set.fromList
    [(DRight, DBottom), (DBottom, DRight), (DLeft, DTop), (DTop, DLeft)]

-- '/'
vertexRight :: DirectionSet
vertexRight =
  Set.fromList
    [(DRight, DTop), (DBottom, DLeft), (DLeft, DBottom), (DTop, DRight)]

railVertical :: DirectionSet
railVertical = Set.fromList [(DBottom, DBottom), (DTop, DTop)]

railHorizontal :: DirectionSet
railHorizontal = Set.fromList [(DRight, DRight), (DLeft, DLeft)]

partialEmpty :: Scenario
partialEmpty = Scenario {carts = [], rails = Map.empty, crosses = Set.empty}

partialAddCross :: (Int, Int) -> Scenario -> Scenario
partialAddCross point (scenario@Scenario {crosses}) =
  scenario {crosses = Set.insert point crosses}

partialAddLRRail :: (Int, Int) -> Scenario -> Scenario
partialAddLRRail = flip partialAddRail vertexLeft

partialAddRLRail :: (Int, Int) -> Scenario -> Scenario
partialAddRLRail = flip partialAddRail vertexRight

partialAddRail :: (Int, Int) -> DirectionSet -> Scenario -> Scenario
partialAddRail point set scenario@Scenario {rails} =
  scenario {rails = Map.insert point set rails}

partialAddHRail :: (Int, Int) -> Scenario -> Scenario
partialAddHRail = flip partialAddRail railHorizontal

partialAddVRail :: (Int, Int) -> Scenario -> Scenario
partialAddVRail = flip partialAddRail railVertical

initCartState :: [Direction]
initCartState = cycle [DLeft, DTop, DRight]

partialAddCart :: (Int, Int) -> Direction -> Scenario -> Scenario
partialAddCart point DTop scenario@Scenario {carts, rails} =
  scenario
  { carts = (point, DTop, initCartState) : carts
  , rails = Map.insert point railVertical rails
  }
partialAddCart point DRight scenario@Scenario {carts, rails} =
  scenario
  { carts = (point, DRight, initCartState) : carts
  , rails = Map.insert point railHorizontal rails
  }
partialAddCart point DBottom scenario@Scenario {carts, rails} =
  scenario
  { carts = (point, DBottom, initCartState) : carts
  , rails = Map.insert point railVertical rails
  }
partialAddCart point DLeft scenario@Scenario {carts, rails} =
  scenario
  { carts = (point, DLeft, initCartState) : carts
  , rails = Map.insert point railHorizontal rails
  }

applyDirection :: (Int, Int) -> Direction -> (Int, Int)
applyDirection (x, y) DBottom = (x, y + 1)
applyDirection (x, y) DLeft = (x - 1, y)
applyDirection (x, y) DRight = (x + 1, y)
applyDirection (x, y) DTop = (x, y - 1)

turn :: Direction -> Direction -> Direction
turn DBottom DLeft = DRight
turn DLeft DLeft = DBottom
turn DRight DLeft = DTop
turn DTop DLeft = DLeft
turn DBottom DRight = DLeft
turn DLeft DRight = DTop
turn DRight DRight = DBottom
turn DTop DRight = DRight
turn dir _ = dir

{-
   Scenario Parsing
 -}
parseStep :: Char -> ((Int, Int), Scenario) -> ((Int, Int), Scenario)
parseStep '\n' ((_, y), partial) = ((0, y + 1), partial)
parseStep ' ' ((x, y), partial) = ((x + 1, y), partial)
parseStep '/' (p@(x, y), partial) = ((x + 1, y), partialAddRLRail p partial)
parseStep '\\' (p@(x, y), partial) = ((x + 1, y), partialAddLRRail p partial)
parseStep '+' (p@(x, y), partial) = ((x + 1, y), partialAddCross p partial)
parseStep '-' (p@(x, y), partial) = ((x + 1, y), partialAddHRail p partial)
parseStep '|' (p@(x, y), partial) = ((x + 1, y), partialAddVRail p partial)
parseStep '^' (p@(x, y), partial) = ((x + 1, y), partialAddCart p DTop partial)
parseStep 'v' (p@(x, y), partial) =
  ((x + 1, y), partialAddCart p DBottom partial)
parseStep '<' (p@(x, y), partial) = ((x + 1, y), partialAddCart p DLeft partial)
parseStep '>' (p@(x, y), partial) =
  ((x + 1, y), partialAddCart p DRight partial)

parse :: String -> Scenario
parse = snd . foldl (flip parseStep) ((0, 0), partialEmpty)

{-
   Problem
-}
duplicates :: Ord a => [a] -> [a]
duplicates =
  Set.toList .
  snd .
  foldl
    (\(all, unique) x ->
       let updatedAll = Set.insert x all
       in if Set.member x all
            then (updatedAll, Set.insert x unique)
            else (updatedAll, unique))
    (Set.empty, Set.empty)

collision :: Scenario -> [(Int, Int)]
collision Scenario {carts} = duplicates $ map (\(a, _, _) -> a) carts

moveCart ::
     (Point, Direction, [Direction])
  -> Scenario
  -> (Point, Direction, [Direction])
moveCart (pos, dir, states) scenario@Scenario {rails, crosses} =
  if Set.member pos crosses
    then let newDir = turn dir $ head states
         in (applyDirection pos newDir, newDir, tail states)
    else case Map.lookup pos rails of
           Nothing -> error "Derail!"
           Just dirSet ->
             let newDir =
                   snd $ head $ filter ((==) dir . fst) $ Set.toList dirSet
             in (applyDirection pos newDir, newDir, states)

getPos :: (a, b, c) -> a
getPos (pos, _, _) = pos

moveCartAtPos :: Point -> Scenario -> Scenario
moveCartAtPos cartPos scenario =
  let cart = head $ filter (\c -> getPos c == cartPos) $ carts scenario
      otherCarts = filter (\c -> getPos c /= cartPos) $ carts scenario
      updatedCart = moveCart cart scenario
      compareCarts lhs rhs = compare (getPos lhs) (getPos rhs)
  in scenario {carts = sortBy compareCarts (updatedCart : otherCarts)}

removeCarts :: [(Int, Int)] -> Scenario -> Scenario
removeCarts cartList scenario@Scenario {carts} =
  scenario {carts = filter (\(p, _, _) -> not $ p `elem` cartList) carts}

findCartAfterCollisions :: Scenario -> [(Int, Int)] -> (Int, Int)
findCartAfterCollisions scenario@Scenario {carts} [] =
  findCartAfterCollisions scenario $ map getPos carts
findCartAfterCollisions scenario (nextCart:cartList) =
  let updatedScenario@Scenario {carts} = moveCartAtPos nextCart scenario
  in if length carts == 1
       then getPos $ head carts
       else let colList = collision updatedScenario
            in findCartAfterCollisions
                 (removeCarts colList updatedScenario)
                 (cartList \\ colList)

main :: IO ()
main = do
  scenario <- parse <$> getContents
  print $ findCartAfterCollisions scenario []
