module Game where

import qualified Command as C
import Control.Monad
import Data.Char
import GameMap
import Solver

data GameState = GameState {map :: Map, status :: Int, bonus :: Int, end :: Coord}

data BallState = BallState
  { pos :: Coord, -- Current position
    posS :: [Coord], -- Prev position stack
    bonusC :: Int, -- Bonus counter
    actionQ :: [C.Action], -- Action queue
    actionS :: [C.Action], -- Prev action stack
    cMap :: Map, -- Current Map
    mapS :: [Map], -- Prev map stack
    bStatus :: Int -- Current status
  }

startGame :: Maybe Map -> IO ()
startGame Nothing = do putStrLn "Error: no map is loaded"
startGame (Just m) = do
  let start = findStart m
      end = findEnd m
   in let map = changeTile "-" start m
       in gameLoop (GameState map 0 0 end) (BallState start [] 0 [] [] map [map] 0)

printMap :: Map -> Coord -> IO ()
printMap m coord = do
  putStr $ unlines (Prelude.map unwords (changeTile "@" coord m))

gameLoop :: GameState -> BallState -> IO ()
gameLoop (GameState _ _ bonus end) (BallState pos poss bc aQ aS m ms s) = do
  if isWin end pos
    then do
      putStrLn "You Win!"
      putStrLn $ "Bonus: " ++ show bonus
    else do
      printMap m pos
      putStrLn "Direction: "
      cmdStr <- getLine
      case words cmdStr of
        ["quit"] -> return ()
        _ -> case C.strToCmd cmdStr of
          Just cmd -> do
            let newState = runner (BallState pos poss bc (C.expandCmd cmd []) aS m ms s)
                (BallState _ _ bc' _ _ _ _ _) = newState
                bonus' = bonus + bc'
            when (bonus' /= bonus) (putStrLn "You got the a bonus!")
            gameLoop (GameState m s bonus' end) newState
          Nothing -> do
            putStrLn "Bad command"
            gameLoop (GameState m s bonus end) (BallState pos poss bc aQ aS m ms s)
  where
    isWin a b = a == b

walkUntil :: C.Direction -> Maybe ColorTile -> Coord -> Map -> Maybe (Coord, Int, Map)
walkUntil act mClr (r, c) m
  | not $ isWalkable (r, c) m = Nothing
  | isStopAtColor (r, c) = Just ((r, c), bonus, m')
  | otherwise = case walkUntil act mClr (getNextPos act (r, c)) m of
    Nothing -> Just ((r, c), bonus, m')
    Just (c, b, m'') -> Just (c, b + bonus, m'')
  where
    bonus = if isBonusTile (r, c) m then 1 else 0
    m' = if isBonusTile (r, c) m then changeTile "-" (r, c) m else m
    isStopAtColor nP = case mClr of
      Nothing -> False
      clr -> tileToColor (at (r, c) m) == clr

runner :: BallState -> BallState
runner (BallState pos posS bonusC actionQ actionS m mS bStat)
  | bStat == -1 || null actionQ = BallState pos posS bonusC actionQ actionS m mS bStat
  | otherwise = case head actionQ of
    C.Action dir -> run dir (tail actionQ)
    C.Condition clr dir ->
      if tileToColor (at pos m) == Just clr
        then run dir (tail actionQ)
        else runner (BallState pos posS bonusC (tail actionQ) actionS m mS bStat)
  where
    run dir as =
      let nextState = walkUntil dir (getColor as) (getNextPos dir pos) m
       in case nextState of
            Nothing -> runner (BallState pos posS bonusC (tail actionQ) actionS m mS (-1))
            Just (nPos, nBc, nM) -> runner (BallState nPos posS (bonusC + nBc) (tail actionQ) actionS nM mS bStat)
    getColor [] = Nothing
    getColor (a : as) = case a of
      C.Condition c _ -> Just c
      _ -> Nothing
