module Solver where

import qualified Command as C
import qualified Data.Graph as G
import qualified Data.IntMap.Lazy as IM
import qualified Data.Map.Lazy as M
import Data.Maybe
import qualified Data.Set as S
import GameMap

type Node = (Coord, Tile)

newtype Graph = Graph [Node]

data Move = Move {action :: C.Direction, tile :: Tile, coord :: Coord} deriving (Show)

type Edge = (Coord, Coord)

getNextPos :: C.Direction -> Coord -> Coord
getNextPos C.Up (r, c) = (r - 1, c)
getNextPos C.Down (r, c) = (r + 1, c)
getNextPos C.Left (r, c) = (r, c - 1)
getNextPos C.Right (r, c) = (r, c + 1)

-- walkUntilBadPos :: C.Direction -> Coord -> Bonus -> Map -> [(Coord, Move)] -> [(Coord, Move)]
-- walkUntilBadPos act (r, c) b m cs =
--   let nextPos = getNextPos act (r, c)
--       nextnextPos = getNextPos act nextPos
--       bonus = if isBonus nextPos then b + 1 else b + 0
--    in if isBadPos nextPos
--         then cs
--         else
--           if isBadPos nextnextPos || isColorTile nextPos m
--             then (nextPos, Move act bonus) : cs
--             else walkUntilBadPos act nextPos bonus m cs
--   where
--     isBadPos pos = not (isValidPos pos m) || isWallTile pos m
--     isBonus pos = at pos m == "b"

-- walkAll :: Coord -> Map -> [Coord]
-- walkAll c map = concatMap walk [C.Up, C.Down, C.Left, C.Right]
--   where
--     walk dir = walkUntilBadPos dir c 0 map []

-- getEdgesRec :: Coord -> Map -> S.Set Coord -> [(Node, Coord, [Coord])] -> [(Node, Coord, [Coord])]
-- getEdgesRec curPos map explored edges =
--   if curPos `S.member` explored
--     then edges
--     else
--       let possible = walkAll curPos map
--           node = makeAdj possible
--           nextWalk c' = getEdgesRec c' map (curPos `S.insert` explored) node
--        in node ++ concatMap nextWalk possible
--   where
--     makeAdj ps = [(makeNode curPos, curPos, ps)]
--     makeNode pos = (pos, at pos map)

-- getEdges :: Coord -> Map -> ([(Node, Coord, [Coord])], M.Map Edge Move)
-- getEdges initPos map = rmdups $ getEdgesRec initPos map S.empty []

rmdups :: [(Node, Coord, [Coord])] -> [(Node, Coord, [Coord])]
rmdups = rmdups' S.empty
  where
    rmdups' _ [] = []
    rmdups' a (b : c) =
      if S.member b a
        then rmdups' a c
        else b : rmdups' (S.insert b a) c

buildGraph edges = let (g, vm) = G.graphFromEdges' edges in g

probe ::
  Coord -> -- Start
  Coord -> -- End
  S.Set Coord -> -- Waypoints (Bonus)
  S.Set Coord -> -- Explored
  Map -> -- Map
  ([Move], Int) ->
  [Maybe ([Move], Int)] -- Move, Score (Number of move + Remaining Bonus * 100)
probe curPos endPos wayPts seen m (moves, score)
  | curPos == endPos = [Just (moves, score + length wayPts * 100)] -- End when it reach target
  | not (isValidPos curPos m) || isWallTile curPos m || S.member curPos seen = [Nothing] -- Branch die when it reach these bad location
  | otherwise -- Keep roll'in
    =
    let (wayPts', m', seen') = updateBonus wayPts curPos m seen
        Move dir _ _ = head moves
        nextPos = getNextPos dir curPos
        nextProbe dir = makeNextProbe dir curPos endPos wayPts' seen' m' (moves, score)
        possibleDir = [C.Up, C.Down, C.Left, C.Right]
     in if isSplit curPos nextPos m then concatMap nextProbe possibleDir else probe nextPos endPos wayPts' seen' m' (moves, score)
  where
    updateBonus wp p m sn = if isBonusTile p m then (S.delete p wp, changeTile "-" p m, S.empty) else (wp, m, sn)
    isBonusTile p m = at p m == "b"
    isSplit cp np m = isColorTile cp m || not (isValidPos np m) || isWallTile np m
    makeNextProbe dir cP eP wPts sn m (ms, sc) = probe (getNextPos dir cP) eP wPts (S.insert cP sn) m (makeMove dir cP m : ms, (sc + 1))
    makeMove dir pos m = Move dir (at pos m) pos

getMinCost :: [([Move], Int)] -> Maybe ([Move], Int)
getMinCost [] = Nothing
getMinCost (m : ms) = getMinCost' ms m
  where
    getMinCost' [] (m, s) = Just (m, s)
    getMinCost' ((m, s) : ms) (m', s') = if s < s' then getMinCost' ms (m, s) else getMinCost' ms (m', s')

startTestProbe :: Map -> Maybe ([Move], Int)
startTestProbe m = getMinCost $ catMaybes (concatMap probe' (firstMoves start))
  where
    probe' move = probe start end bonusS S.empty m move
    start = findStart m
    end = findEnd m
    bonusS = S.fromList (findBonus m)
    firstMoves pos = map (\dir -> ([Move dir (at pos m) pos], 1)) (filter (\dir -> isWalkable (getNextPos dir pos) m) [C.Up, C.Down, C.Left, C.Right])
