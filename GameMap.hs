module GameMap where

import Data.List (elemIndices)

type Map = [[Tile]]

type Tile = String

type Coord = (Int, Int)

type Bonus = Int

data ColorTile = Pink | Yellow | Green deriving (Show)

-- Find positions of a tile
findTileCoords :: Map -> Tile -> [Coord]
findTileCoords m t = [(r, c) | (r, line) <- zip [0 ..] m, c <- elemIndices t line]

-- Find and return the position of the first match of a tile
findTile :: Map -> Tile -> Coord
findTile m t = head $ findTileCoords m t

findStart :: Map -> Coord
findStart m = findTile m "@"

findEnd :: Map -> Coord
findEnd m = findTile m "t"

findBonus :: Map -> [Coord]
findBonus m = findTileCoords m "b"

-- Unsafe: Coordinate must be checked before use it
-- Access and get the tile by a coordinate
at :: Coord -> Map -> Tile
at (r, c) map = map !! r !! c

-- Check if a coordinate within the range of a map
isValidPos :: Coord -> Map -> Bool
isValidPos (r, c) m =
  isInRange r (-1) (length m)
    && isInRange c (-1) (length (head m))
  where
    isInRange v min max = v > min && v < max

-- Unsafe: Coordinate must be checked before use it
-- Check if a tile of a coordinate is wall
isWallTile :: Coord -> Map -> Bool
isWallTile c m = at c m == "*"

-- Check if a tile of a coordinate is walkable, ie: valid pos and not a wall tile
isWalkable :: Coord -> Map -> Bool
isWalkable c m = isValidPos c m && not (isWallTile c m)

-- Get the color of a tile
tileToColor :: Tile -> Maybe ColorTile
tileToColor "y" = Just Yellow
tileToColor "p" = Just Pink
tileToColor "g" = Just Green
tileToColor _ = Nothing

-- Check if the a tile colored
isColorTile :: Coord -> Map -> Bool
isColorTile c m = case tileToColor (at c m) of
  Nothing -> False
  Just _ -> True
  
-- Unsafe: Coordinate must be checked before use it
-- Replace a tile in a map with a new symbol and then return the new map
changeTile :: Tile -> Coord -> Map -> Map
changeTile t (r, c) m =
  take r m
    ++ [take c (m !! r) ++ [t] ++ drop (c + 1) (m !! r)]
    ++ drop (r + 1) m

testStr = ["* * * * * - - - - - - - - - - - - - - - - - - - * * * * *", "* * * * * b - - - - - - - - - - - - - - - - - b * * * * *", "* * * * * - * * * * * * * * * * * * * * * * * - * * * * *", "* * * * * - * * - - - - * * * * * - - - - * * - * * * * *", "* * * * * - * * - y y - * * * * * - y y - * * - * * * * *", "* * * * * - * * - - - - * * * * * - - - - * * - * * * * *", "* * * * * - * * * * * * - - b - - * * * * * * - * * * * *", "* * * * * - * * * * * * - * * * - * * * * * * - * * * * *", "@ - - - - - * * * * * * - * * * - * * * * * * p - - - - t", "* * * * * - * * * * * * - * * * - * * * * * * - * * * * *", "* * * * * - - - - - - - - * * * - - - - - - - - * * * * *", "* * * * * * * * * * * * * * * * * * * * * * * * * * * * *", "* * * * * * * * * * * * * * * * * * * * * * * * * * * * *"]

testStr2 = ["* * * * * * * * * * *", "* * @ - - b - - - * *", "* * - * * * * * - * *", "- - y * * * * * y - t", "* * - * * * * * - * *", "* * - - - b - - - * *", "* * * * * * * * * * *"]

testMap = Prelude.map words testStr

testMap2 = Prelude.map words testStr2
