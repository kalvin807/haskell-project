module GameMap where

import Data.List (elemIndices)

type Map = [[Tile]]

type Tile = String

type Coord = (Int, Int)

type Bonus = Int

data ColorTile = Blue | Pink | Yellow

findTileCoords :: Map -> Tile -> [Coord]
findTileCoords m t =  [(r, c) | (r, line) <- zip [0 ..] m, c <- elemIndices t line]

findTile ::  Map -> Tile -> Coord
findTile m t = head $ findTileCoords m t

findStart :: Map -> Coord
findStart m = findTile m "@"

findEnd:: Map -> Coord
findEnd m = findTile m "t"

findBonus:: Map -> [Coord]
findBonus m = findTileCoords m "b"

at :: Coord -> Map -> Tile
at (r, c) map = map !! r !! c

isValidPos :: Coord -> Map -> Bool
isValidPos (r, c) m =
  isInRange r (-1) (length m)
    && isInRange c (-1) (length (head m))
  where
    isInRange v min max = v > min && v < max

isWallTile :: Coord -> Map -> Bool
isWallTile c m = at c m == "*"

isWalkable :: Coord -> Map -> Bool
isWalkable c m = isValidPos c m && not (isWallTile c m)

isColorTile :: Coord -> Map -> Bool
isColorTile c m = case at c m of
  "p" -> True
  "y" -> True
  _ -> False

changeTile :: Tile -> Coord -> Map -> Map
changeTile t (r, c) m =
  take r m
    ++ [take c (m !! r) ++ [t] ++ drop (c + 1) (m !! r)]
    ++ drop (r + 1) m

 


-- Need to check if there is a path from @ -> t
-- A valid map need to have:
-- 1 ball (@)
-- 1 target (t)
-- at least 1 path from ball (@) to target (t)
validate :: Map -> Bool
validate m = countTile "@" m == 1 && countTile "t" m == 1
  where
    countTile t m = sum [1 | xs <- m, x <- xs, t == x]

testStr = ["* * * * * - - - - - - - - - - - - - - - - - - - * * * * *", "* * * * * b - - - - - - - - - - - - - - - - - b * * * * *", "* * * * * - * * * * * * * * * * * * * * * * * - * * * * *", "* * * * * - * * - - - - * * * * * - - - - * * - * * * * *", "* * * * * - * * - y y - * * * * * - y y - * * - * * * * *", "* * * * * - * * - - - - * * * * * - - - - * * - * * * * *", "* * * * * - * * * * * * - - b - - * * * * * * - * * * * *", "* * * * * - * * * * * * - * * * - * * * * * * - * * * * *", "@ - - - - - * * * * * * - * * * - * * * * * * p - - - - t", "* * * * * - * * * * * * - * * * - * * * * * * - * * * * *", "* * * * * - - - - - - - - * * * - - - - - - - - * * * * *", "* * * * * * * * * * * * * * * * * * * * * * * * * * * * *", "* * * * * * * * * * * * * * * * * * * * * * * * * * * * *"]

testStr2 = ["* * * * * * * * * * *", "* * @ - - b - - - * *", "* * - * * * * * - * *", "- - y * * * * * y - t", "* * - * * * * * - * *", "* * - - - b - - - * *", "* * * * * * * * * * *"]

testMap = Prelude.map words testStr

testMap2 = Prelude.map words testStr2
