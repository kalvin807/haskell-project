data Game = Game
  { map :: Maybe [String],
    status :: Int
  }

loadMap :: String -> IO [String]
loadMap path = do
  contents <- readFile path
  return $ lines contents


printIntro :: IO ()
printIntro = do
  putStrLn "dfgsdhfghdfghdfgh"

mainLoop :: Game -> IO ()
mainLoop (Game _ 0) = do return ()
mainLoop (Game m s) = do
  putStrLn "ok(?): load, quit | todo: play, check, solve"
  cmd <- getLine
  case words cmd of
    ["quit"] -> mainLoop (Game m 0)
    ["load", path] -> do
      putStrLn path
      map <- loadMap path
      mainLoop (Game (Just map) 0)
    _ -> do
      putStrLn $ "Invalid option -- '" ++ cmd ++ "'"
      putStrLn "Try 'help' for more information."
      mainLoop (Game m s)

main :: IO ()
main = do
  printIntro
  mainLoop (Game Nothing 1)
