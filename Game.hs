module Game where
import Data.Char
import GameMap

import qualified Command as C

data GameState = GameState {map :: Map, status :: Int, bonus :: Integer, ball :: Coord}

getNextPos :: C.Direction -> Coord -> Coord
getNextPos C.Up (r, c) = (r - 1, c)
getNextPos C.Down (r, c) = (r + 1, c)
getNextPos C.Left (r, c) = (r, c - 1)
getNextPos C.Right (r, c) = (r, c + 1)

walkUntilWall :: C.Direction -> Coord -> Map -> Coord
walkUntilWall act (r, c) m =
  let nextPos = getNextPos act (r, c)
   in if isValidPos nextPos m && not (isWallTile nextPos m)
        then walkUntilWall act nextPos m
        else (r, c)
  where
    isWallTile c m = at c m == "*"

startGame :: Maybe Map -> IO ()
startGame Nothing = do putStrLn "Error: no map is loaded"
startGame (Just m) = do
  let initCoord = findStart m
   in let map = changeTile "-" initCoord m
       in gameLoop (GameState map 0 0 initCoord)

printMap :: Map -> Coord -> IO ()
printMap m coord = do
  putStr $ unlines (Prelude.map unwords (changeTile "@" coord m))

praseCmd :: [String] -> Maybe C.Direction
praseCmd ["left"] = Just C.Left
praseCmd ["right"] = Just C.Right
praseCmd ["up"] = Just C.Up
praseCmd ["down"] = Just C.Down
praseCmd [_] = Nothing

gameLoop :: GameState -> IO ()
gameLoop (GameState m s bonus ball) = do
  printMap m ball
  putStrLn "Enter your direction"
  cmdStr <- getLine
  let cmd = praseCmd $ words (toLower' cmdStr)
   in case cmd of
        Just action -> do 
          let (x,y) = walkUntilWall action ball m
          putStrLn $ show x ++ show y
          gameLoop (GameState m s bonus (x,y))
        Nothing -> do
          putStrLn "Bad command"
          gameLoop (GameState m s bonus ball)
  where
    toLower' str = [toLower x | x <- str]
