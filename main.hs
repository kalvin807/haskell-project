import Game
import GameMap (Map)
import System.Directory (doesFileExist, getCurrentDirectory)

data LoadState = LoadState
  { map :: Maybe Map,
    status :: Int
  }

loadMap :: String -> IO (Maybe [[String]])
loadMap fileName = do
  exedir <- getCurrentDirectory
  let fullpath = exedir ++ "/" ++ removeQuote fileName
  putStrLn fullpath
  isFileExist <- doesFileExist fullpath
  if isFileExist
    then do
      contents <- readFile fullpath
      return $ Just (Prelude.map words (lines contents))
    else do
      putStrLn "The map file does not exist!"
      return Nothing
  where
    removeQuote xs = [x | x <- xs, x `notElem` "\""]

printIntro :: IO ()
printIntro = do
  putStrLn "WELCOME TO HASKELL PROJECT"

mainLoop :: LoadState -> IO ()
mainLoop (LoadState _ 0) = do return ()
mainLoop (LoadState m s) = do
  putStrLn "ok(?): load, quit | testing: play | todo: check, solve"
  cmd <- getLine
  case words cmd of
    ["quit"] -> mainLoop (LoadState m 0)
    ["load", path] -> do
      map <- loadMap path
      mainLoop (LoadState map s)
    ["play"] -> do
      startGame m
    _ -> do
      putStrLn $ "Invalid option -- '" ++ cmd ++ "'"
      putStrLn "Try 'help' for more information."
      mainLoop (LoadState m s)

main :: IO ()
main = do
  printIntro
  mainLoop (LoadState Nothing 1)
