import Command
import qualified Control.Monad
import Game
import GameMap (Map)
import Solver (solve, validate)
import System.Directory (doesFileExist, getCurrentDirectory)
import Util

data LoadState = LoadState
  { map :: Maybe (Map, String),
    status :: Int
  }

loadMap :: String -> IO (Maybe (Map, String))
loadMap fileName = do
  exedir <- getCurrentDirectory
  let fullpath = exedir ++ "/" ++ removeQuote fileName
  putStrLn fullpath
  isFileExist <- doesFileExist fullpath
  if isFileExist
    then do
      contents <- readFile fullpath
      return $ Just (Prelude.map words (lines contents), fileName)
    else do
      putStrLn "The map file does not exist!"
      return Nothing
  where
    removeQuote xs = [x | x <- xs, x `notElem` "\""]

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

mainLoop :: LoadState -> IO ()
mainLoop (LoadState _ 0) = do Control.Monad.void (putStrLn "Bye!")
mainLoop (LoadState m s) = do
  putStrLn ""
  putStrLn "'load <filename>' - Load a map file (at root folder) | 'check' - Validate the loaded map | 'solve' - Solve a loaded map"
  putStrLn "'play' - Start interactive play | 'quit' - Quit Kodable"
  putStrLn $ "MAP: " ++ case m of Nothing -> "NOT LOADED"; Just (_, n) -> n
  cmd <- prompt "|kodable> "
  case words cmd of
    ["quit"] -> mainLoop (LoadState m 0)
    ["load", path] -> do
      map <- loadMap path
      mainLoop (LoadState map s)
    ["check"] -> do
      case m of
        Nothing -> do mainLoop (LoadState m s)
        Just (map, _) -> do
          if validate map
            then do
              putStrLn "The loaded map is valid."
            else do
              putStrLn "The loaded map is invalid!"
          mainLoop (LoadState m s)
    ["solve"] -> do
      case m of
        Nothing -> do mainLoop (LoadState m s)
        Just (map, _) -> do
          if validate map
            then do
              print $ solve map
            else do
              putStrLn "Error: Not a valid map!"
          mainLoop (LoadState m s)
    "play" : ws -> do
      case m of
        Nothing -> do
          putStrLn "Error: Not loaded any map!"
          mainLoop (LoadState m s)
        Just (map, _) -> do
          case parseFunction (unwords ws) of
            Nothing -> putStrLn "Error: Incorrect function"
            Just cs -> if length cs > 2 then do putStrLn "Error: Function too long (at most 2)" else startGame map cs
          mainLoop (LoadState m s)
    _ -> do
      putStrLn $ "Error: Invalid/Incomplete option -- '" ++ cmd ++ "'"
      mainLoop (LoadState m s)

main :: IO ()
main = do
  printIntro
  mainLoop (LoadState Nothing 1)
