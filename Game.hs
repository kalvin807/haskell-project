module Game where

import Command
import Control.Monad
import Data.Char
import GameMap
import Solver
import Util

data GameState = GameState {maxBonus :: Int, bonus :: Int, end :: Coord, func:: [Command]}

data BallState = BallState
  { pos :: Coord, -- Current position
    posS :: [Coord], -- Prev position stack
    bonusC :: [Int], -- Bonus counter
    actionQ :: [Action], -- Action queue
    cMap :: Map, -- Current Map
    mapS :: [Map], -- Prev map stack
    bStatus :: Int -- Current status
  }

printMap :: Map -> Coord -> IO ()
printMap m coord = do
  putStrLn udFrame
  putStr $ unlines (Prelude.map (lrFrame . unwords . Prelude.map cliColor) (changeTile "@" coord m))
  putStrLn udFrame
  where
    lrFrame str = "\27[0m|| " ++ str ++ " \27[0m||"
    udFrame = lrFrame . unwords $ replicate (length $ head m) "="

printWin :: Int -> Int -> IO ()
printWin bonus mBonus = do
  putStrLn "You Win!"
  putStrLn $ "Bonus: " ++ ppBonus bonus mBonus ++ bonusMsg bonus mBonus
  where
    ppBonus b mb = "[ " ++ concat (replicate b "\27[0m*") ++ concat (replicate (mb - b) "\27[30m*") ++ " ]"
    bonusMsg b mb
      | mb - b == 0 = "Well Done!"
      | mb - b <= 2 = "Not Bad!"
      | otherwise = "Keep going!"

runner :: BallState -> BallState
runner (BallState pos posS bonusC actionQ m mS bStat)
  | bStat == -1 || null actionQ = BallState pos posS bonusC actionQ m mS bStat
  | otherwise = case head actionQ of
    Action dir -> run dir (tail actionQ)
    Condition clr dir ->
      if tileToColor (at pos m) == Just clr
        then run dir (tail actionQ)
        else runner (BallState pos posS' bonusC (tail actionQ) m mS' bStat)
  where
    run dir as =
      let nextState = walkUntil dir (getColor as) (getNextPos dir pos) m
       in case nextState of
            Nothing -> runner (BallState pos posS bonusC actionQ m mS (-1))
            Just (nPos, nBc, nM) -> runner (BallState nPos posS' (nBc + head bonusC : bonusC) (tail actionQ) nM mS' bStat)
    posS' = pos : posS
    mS' = m : mS
    getColor [] = Nothing
    getColor (a : as) = case a of
      Condition c _ -> Just c
      _ -> Nothing

gameLoop :: GameState -> BallState -> IO ()
gameLoop (GameState mBonus bonus end func) (BallState pos posS bc aQ m mS s) = do
  if isWin end pos
    then printWin bonus mBonus
    else do
      printMap m pos
      putStrLn "Enter direction(s) | 'quit' - Exit this map | 'undo' - Undo a direction | 'hint' - A hint that lead you to target ;) "
      cmdStr <- prompt "|Direction(s)> "
      case words cmdStr of
        ["quit"] -> return ()
        ["undo"] ->
          if null posS
            then do
              putStrLn "Error: No more undo left"
              gameLoop (GameState mBonus bonus end func) (BallState pos posS bc aQ m mS s)
            else do
              let lastMap = head mS
                  lastPos = head posS
                  lastB = head bc
               in gameLoop (GameState mBonus lastB end func) (BallState lastPos (tail posS) (tail bc) aQ lastMap (tail mS) s)
        ["hint"] ->
          do
            putStrLn $ "How about... " ++ (show . head $ validateSolve (changeTile "@" pos m))
            gameLoop (GameState mBonus bonus end func) (BallState pos posS bc aQ m mS s)
        _ -> case parseCommand cmdStr of
          Just cmd -> do
            let newState = runner (BallState pos posS bc (expandCmd cmd (expandCmd func [])) m mS 0)
                (BallState _ _ bc' aQ _ _ s') = newState
            when (bonus /= head bc') (putStrLn "You got the a bonus!")
            when (s' == (-1)) (putStrLn $ "Error: You cannot do " ++ show (head aQ))
            gameLoop (GameState mBonus (head bc') end func) newState
          Nothing -> do
            putStrLn "Error: Bad command"
            gameLoop (GameState mBonus bonus end func) (BallState pos posS bc aQ m mS s)
  where
    isWin a b = a == b

walkUntil :: Direction -> Maybe ColorTile -> Coord -> Map -> Maybe (Coord, Int, Map)
walkUntil act mClr (r, c) m
  | not $ isWalkable (r, c) m = Nothing
  | isStopAtColor (r, c) = Just ((r, c), bonus, m')
  | otherwise = case walkUntil act mClr (getNextPos act (r, c)) m' of
    Nothing -> Just ((r, c), bonus, m')
    Just (c, b, m'') -> Just (c, b + bonus, m'')
  where
    bonus = if isBonusTile (r, c) m then 1 else 0
    m' = if isBonusTile (r, c) m then changeTile "-" (r, c) m else m
    isStopAtColor nP = case mClr of
      Nothing -> False
      clr -> tileToColor (at (r, c) m) == clr

startGame :: Map -> [Command] -> IO ()
startGame m func = do
  let start = findStart m
      end = findEnd m
   in let map = changeTile "-" start m
       in gameLoop (GameState (length $ findBonus m) 0 end func) (BallState start [start] [0] [] map [map] 0)
