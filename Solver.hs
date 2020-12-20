module Solver where

import Command
import Data.Maybe
import qualified Data.Set as S
import GameMap

data Move = Move {action :: Direction, tile :: Tile, coord :: Coord} deriving (Show)

-- Helper function to get the path with the lowest cost
getMinCost :: [([Move], Int)] -> Maybe [Move]
getMinCost [] = Nothing
getMinCost (m : ms) = getMinCost' ms m
  where
    getMinCost' [] (m, s) = Just m
    getMinCost' ((m, s) : ms) (m', s') = if s < s' then getMinCost' ms (m, s) else getMinCost' ms (m', s')

-- Probe to exhaust all possible way to reach end point with cost calculated
probe ::
  Coord -> -- Start
  Coord -> -- End
  S.Set Coord -> -- Waypoints (Bonus)
  S.Set Coord -> -- Explored
  Map -> -- Map
  ([Move], Int) -> -- Moves, Cost
  [Maybe ([Move], Int)] -- Moves, cost (Number of move + Remaining Bonus * 100)
probe curPos endPos wayPts seen m (moves, cost)
  | curPos == endPos = [Just (moves, cost + length wayPts * 100)] -- End when it reach target
  | not (isWalkable curPos m) || S.member curPos seen = [Nothing] -- Branch die when it reach these bad location
  | otherwise -- Keep roll'in
    =
    let -- When it collect a bonus, remove it from waypoint set and reset the seen set
        (wayPts', m', seen') = updateIfBonus wayPts curPos m seen
        -- Last move
        Move dir _ _ = head moves
        -- Next coordinate if continue to follow the previous move
        nextPos = getNextPos dir curPos
        -- Partial function for mapping out new probes
        nextProbe dir = makeNextProbe dir curPos endPos wayPts' seen' m' (moves, cost)
     in -- If current position is allow to change direction
        if isSplit curPos nextPos m
          then -- Then generate probes to search the new directions
            concatMap nextProbe allDirection
          else -- Otherwise continue with the same direction
            probe nextPos endPos wayPts' seen' m' (moves, cost)
  where
    updateIfBonus wp p m sn = if isBonusTile p m then (S.delete p wp, changeTile "-" p m, S.empty) else (wp, m, sn)
    isSplit cp np m = isColorTile cp m || not (isWalkable np m)
    makeNextProbe dir cP eP wPts sn m (ms, c) = probe (getNextPos dir cP) eP wPts (S.insert cP sn) m (makeMove dir cP m : ms, c + 1)
    makeMove dir pos m = Move dir (at pos m) pos

-- TODO: Refactor to gen moves for any case?
-- Helper function that generate all possible direction at the beginning
firstMoves :: Map -> Coord -> [([Move], Int)]
firstMoves m pos = map (\dir -> ([Move dir (at pos m) pos], 1)) (filter (\dir -> isWalkable (getNextPos dir pos) m) allDirection)

-- Translate moves to list of action that can further translated to a set of instruction
movesToActions :: Maybe [Move] -> [Action]
movesToActions Nothing = []
movesToActions (Just ms) = ms2as ms
  where
    ms2as [] = []
    ms2as (m : ms) = m2a m : ms2as ms
    m2a (Move dir t _) = case tileToColor t of
      Just c -> Condition c dir
      Nothing -> Action dir

-- Unsafe: map must be validated before use
-- Solve and give path that try to collect all bonus and reach endpoint
solve :: Map -> [Action]
solve m = reverse $ movesToActions (getMinCost (catMaybes (concatMap probe' (firstMoves m start))))
  where
    probe' move = probe start end bonusS S.empty m move
    start = findStart m
    end = findEnd m
    bonusS = S.fromList (findBonus m)

-- Unsafe: map must be validated before use
-- Solve and give path that try to reach endpoint
validateSolve :: Map -> [Action]
validateSolve m = reverse $ movesToActions (getMinCost (catMaybes (concatMap probe' (firstMoves m start))))
  where
    probe' move = probe start end S.empty S.empty m move
    start = findStart m
    end = findEnd m

-- A valid map need to have:
-- no invalid symbol
-- 1 ball (@)
-- 1 target (t)
-- at least 1 path from ball (@) to target (t)
-- Check if a map valid
validate :: Map -> Bool
validate m = isTilesValid m && ballCount == 1 && endCount == 1 && paths > 0
  where
    ballCount = length $ findTileCoords m "@"
    endCount = length $ findTileCoords m "t"
    paths = length $ validateSolve m
    isTilesValid = all (all isValidTile)
    isValidTile = flip elem ["*", "-", "@", "t", "b", "g", "p", "y"]
