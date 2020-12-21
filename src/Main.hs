-- |
-- Module      :  Main.hs
-- Description :  Entry point of Kodable
-- Copyright   :  Leung Chun Yin
-- License     :  MIT
--
-- Maintainer  :  kalvin80@hku.hk
-- Stability   :  experimental
-- Portability :  portable
--
-- Entry point of the game. It handle the IO of loading game and the lobby screen.
module Main where

import Command (parseFunction)
import qualified Control.Monad
import Game (printMap, startGame)
import GameMap (Map, validate)
import Solver (isSolvable, solve)
import System.Directory (doesFileExist, getCurrentDirectory)
import Util (prompt)

data LoadState = LoadState
  { map :: Maybe (Map, String),
    status :: Int
  }

-- Try to load a map by user input
-- Nothing when the file does not exist or it is invalid
-- Map with file name if it is validated (not necessary solvable)
loadMap :: String -> IO (Maybe (Map, String))
loadMap fileName = do
  exedir <- getCurrentDirectory
  let fullpath = exedir ++ "/map/" ++ removeQuote fileName
  isFileExist <- doesFileExist fullpath
  if isFileExist
    then do
      contents <- readFile fullpath
      let (m, name) = (Prelude.map words (lines contents), fileName)
       in if validate m
            then do
              putStrLn "Read map successfully!"
              putStrLn "Initial:"
              printMap m
              return $ Just (m, name)
            else do
              putStrLn "Error: Invalid map"
              return Nothing
    else do
      putStrLn "Error: The map file does not exist!"
      return Nothing
  where
    removeQuote xs = [x | x <- xs, x `notElem` "\""]

-- One time print to welcome player
-- Note: Repetitive code for easier formatting
printIntro :: IO ()
printIntro = do
  putStrLn "b - WELCOME TO - - - - - - - - - - - - - - - - - - - - - - - - - - - - b"
  putStrLn "* -  ##  ###   ## ##   ### ##     ##     ### ##   ####     ### ###   - *"
  putStrLn "* -  ##  ##   ##   ##   ##  ##     ##     ##  ##   ##       ##  ##   - *"
  putStrLn "* -  ## ##    ##   ##   ##  ##   ## ##    ##  ##   ##       ##       - *"
  putStrLn "* -  ## ##    ##   ##   ##  ##   ##  ##   ## ##    ##       ## ##    - *"
  putStrLn "* -  ## ###   ##   ##   ##  ##   ## ###   ##  ##   ##       ##       - *"
  putStrLn "* -  ##  ##   ##   ##   ##  ##   ##  ##   ##  ##   ##  ##   ##  ##   - *"
  putStrLn "* -  ##  ###   ## ##   ### ##   ###  ##  ### ##   ### ###  ### ###   - *"
  putStrLn "@ - By Leung Chun Yin - 3035437939 - COMP3258 Functional Programming - t"

-- IO when it checks the map
doCheck :: Map -> String -> IO ()
doCheck m _ =
  if isSolvable m
    then do
      putStrLn "The loaded map is valid."
    else do
      putStrLn "The loaded map is unsolvable!"

-- IO when it solves map and gives solution
doSolve :: Map -> String -> IO ()
doSolve m _ =
  if validate m
    then do
      case solve m of
        (cmd, []) -> putStrLn $ showCmd cmd
        (cmd, funcs) -> putStrLn $ showCmd cmd ++ " with " ++ showCmd funcs
    else do
      putStrLn "Error: Not a valid map!"
  where
    showCmd cs = unwords $ Prelude.map show cs

-- IO helper to check map is loaded
checkLoadnRun :: Maybe (Map, String) -> (Map -> String -> IO ()) -> IO ()
checkLoadnRun ms func = case ms of
  Nothing -> do putStrLn "Error: Not loaded any map!"
  Just (m, s) -> func m s

-- Lobby screen loop
mainLoop :: LoadState -> IO ()
mainLoop (LoadState _ 0) = do Control.Monad.void (putStrLn "Bye!")
mainLoop (LoadState m s) = do
  putStrLn ""
  putStrLn "'load <filename>' - Load a map file (at root folder) | 'check' - Validate the loaded map | 'solve' - Solve a loaded map"
  putStrLn "'play optional:<function>' - Start interactive play with a optional function| 'quit' - Quit Kodable"
  putStrLn $ "MAP: " ++ case m of Nothing -> "NOT LOADED"; Just (_, n) -> n
  cmd <- prompt "|kodable> "
  case words cmd of
    ["quit"] -> mainLoop (LoadState m 0)
    ["load", path] -> do
      map <- loadMap path
      mainLoop (LoadState map s)
    ["check"] -> do
      checkLoadnRun m doCheck
      mainLoop (LoadState m s)
    ["solve"] -> do
      checkLoadnRun m doSolve
      mainLoop (LoadState m s)
    "play" : ws -> do
      case m of
        Nothing -> do
          putStrLn "Error: Not loaded any map!"
          mainLoop (LoadState m s)
        Just (map, _) -> do
          case parseFunction (unwords ws) of
            Nothing -> putStrLn "Error: Incorrect function"
            Just cs -> if length cs > 3 then do putStrLn "Error: Function too long (at most 3)" else startGame map cs
          mainLoop (LoadState m s)
    _ -> do
      putStrLn $ "Error: Invalid/Incomplete option -- '" ++ cmd ++ "'"
      mainLoop (LoadState m s)

-- Entry point
main :: IO ()
main = do
  printIntro
  mainLoop (LoadState Nothing 1)
