-- |
-- Module      :  Game.hs
-- Description :  Handling interactive play of Kodable
-- Copyright   :  Leung Chun Yin
-- License     :  MIT
--
-- Maintainer  :  kalvin80@hku.hk
-- Stability   :  experimental
-- Portability :  portable
--
-- Game module encapsulate all game logic need to have interactive play in kodable.
-- It include a loop that listen user command and functions that react to the commands.
-- It currently support 'quit', 'undo', 'hint', and command as defined in Command module.
module Game where

import Command
  ( Action (..),
    Command,
    Direction,
    expandCmd,
    getNextPos,
    parseCommand,
  )
import Control.Monad (when)
import GameMap
  ( ColorTile,
    Coord,
    Map,
    at,
    changeTile,
    findBonus,
    findEnd,
    findStart,
    isBonusTile,
    isWalkable,
    tileToColor,
  )
import Solver (validateSolve)
import Util (cliColor, prompt)

data GameState = GameState {maxBonus :: Int, bonus :: Int, end :: Coord, func :: [Command]}

data BallState = BallState
  { pos :: Coord, -- Current position
    posS :: [Coord], -- Prev position stack
    bonusC :: [Int], -- Bonus counter
    actionQ :: [Action], -- Action queue
    cMap :: Map, -- Current Map
    mapS :: [Map], -- Prev map stack
    bStatus :: Int -- Current status
  }

-- Print a map with color
printMap :: Map -> IO ()
printMap m = do
  putStrLn udFrame
  putStr $ unlines (Prelude.map (lrFrame . unwords . Prelude.map cliColor) m)
  putStrLn udFrame
  where
    lrFrame str = "\27[0m|| " ++ str ++ " \27[0m||"
    udFrame = lrFrame . unwords $ replicate (length $ head m) "="

-- Print the ending message
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

-- A probe to run user specified actions
runner :: BallState -> BallState
runner (BallState pos posS bonusC actionQ m mS bStat)
  -- Stop when the runner is in error (-1) or no more action in queue
  | bStat == -1 || null actionQ = BallState pos posS bonusC actionQ m mS bStat
  -- Otherwise execute the first action in the queue
  | otherwise = case head actionQ of
    -- If it is a action, simply run it
    Action dir -> run dir (tail actionQ)
    -- Else if it is a color conditional action, only run if the color match current tile
    Condition clr dir ->
      if tileToColor (at pos m) == Just clr
        then run dir (tail actionQ)
        else runner (BallState pos posS' bonusC (tail actionQ) m mS' bStat)
  where
    -- Partial function to run the probe recursively
    run dir as =
      -- Execute a walk
      let nextState = walkUntil dir (getColor as) (getNextPos dir pos) m
       in case nextState of
            -- Nothing means the walk failed
            Nothing -> runner (BallState pos posS bonusC actionQ m mS (-1))
            Just (nPos, nBc, nM) -> runner (BallState nPos posS' (nBc + head bonusC : bonusC) (tail actionQ) nM mS' bStat)
    posS' = pos : posS
    mS' = m : mS
    getColor [] = Nothing
    getColor (a : as) = case a of
      Condition c _ -> Just c
      _ -> Nothing

-- Main loop of the interactive game, handles most of the IO
gameLoop :: GameState -> BallState -> IO ()
gameLoop (GameState mBonus bonus end func) (BallState pos posS bc aQ m mS s) = do
  if isWin end pos
    then printWin bonus mBonus
    else do
      printMap' m pos
      putStrLn "Enter direction(s) | 'quit' - Exit this map | 'undo' - Undo a direction | 'hint' - A hint that lead you to target ;) "
      cmdStr <- prompt "|Direction(s)> "
      case words cmdStr of
        -- Special commands [quit, undo, hint]
        ["quit"] -> return ()
        ["undo"] ->
          -- If the position stack is empty, stop the undo
          if null posS
            then do
              putStrLn "Error: No more undo left"
              gameLoop (GameState mBonus bonus end func) (BallState pos posS bc aQ m mS s)
            else do
              -- Undo by retrieving last state from the stacks
              let lastMap = head mS
                  lastPos = head posS
                  lastB = head bc
               in gameLoop (GameState mBonus lastB end func) (BallState lastPos (tail posS) (tail bc) aQ lastMap (tail mS) s)
        ["hint"] ->
          -- Provide the hint by computing the route based on current map
          do
            putStrLn $ "How about... " ++ (show . head $ validateSolve (changeTile "@" pos m))
            gameLoop (GameState mBonus bonus end func) (BallState pos posS bc aQ m mS s)
        -- Try to parse input as commands
        _ -> case parseCommand cmdStr of
          -- Execute if parse success
          Just cmd -> do
            let newState = runner (BallState pos posS bc (expandCmd cmd (expandCmd func [])) m mS 0)
                (BallState _ _ bc' aQ _ _ s') = newState
            when (bonus /= head bc') (putStrLn "You got the a bonus!")
            when (s' == (-1)) (putStrLn $ "Error: Cannot move to " ++ show (head aQ))
            gameLoop (GameState mBonus (head bc') end func) newState
          -- Stop of parse fail
          Nothing -> do
            putStrLn "Error: Invalid command"
            gameLoop (GameState mBonus bonus end func) (BallState pos posS bc aQ m mS s)
  where
    isWin a b = a == b
    printMap' m coord = printMap (changeTile "@" coord m)

-- Walk recursively until the ball is at point of interest (before wall or at color tile)
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

-- Driver function to start the loop
startGame :: Map -> [Command] -> IO ()
startGame m func = do
  let start = findStart m
      end = findEnd m
   in let map = changeTile "-" start m
       in gameLoop (GameState (length $ findBonus m) 0 end func) (BallState start [start] [0] [] map [map] 0)
