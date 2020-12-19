module Command where

import Control.Applicative
import Data.Char
import GameMap
import Parser

data Direction = Up | Down | Left | Right deriving (Show)

data Action = Action Direction | Condition ColorTile Direction

data Command = Command Action | Loop Int Action Action | Function

instance Show Action where
  show (Action dir) = show dir
  show (Condition clr dir) = "Cond{" ++ show clr ++ "}{" ++ show dir ++ "}"

instance Show Command where
  show (Command a) = show a
  show (Loop i a1 a2) = "Loop{" ++ show i ++ "}{" ++ show a1 ++ "," ++ show a2 ++ "}"
  show Function = "Function"

allDirection :: [Direction]
allDirection = [Up, Down, Command.Left, Command.Right]

colorP :: Parser ColorTile
colorP = choose [pinkP, yellowP, greenP]
  where
    pinkP = char 'p' >> return Pink
    yellowP = char 'y' >> return Yellow
    greenP = char 'g' >> return Green

dirP :: Parser Direction
dirP = choose [upP, downP, leftP, rightP]
  where
    upP = string "up" >> return Up
    downP = string "down" >> return Down
    leftP = string "left" >> return Command.Left
    rightP = string "right" >> return Command.Right

actDirP :: Parser Action
actDirP = do Action <$> dirP

condP :: Parser Action
condP = do
  string "cond"
  c <- char '{' *> colorP <* char '}'
  d <- char '{' *> dirP <* char '}'
  return (Condition c d)

actP :: Parser Action
actP = choose [condP, actDirP]

loopP :: Parser Command
loopP = do
  _ <- string "loop"
  c <- char '{' *> satisfy isLoopInt <* char '}'
  char '{'
  a1 <- actP
  _ <- char ','
  a2 <- actP
  char '}'
  return $ Loop (digitToInt c) a1 a2
  where
    isLoopInt = flip elem "12345"

cmdActP :: Parser Command
cmdActP = do Command <$> actP

funcP :: Parser Command
funcP = string "function" >> return Function

commandP' :: Parser Command
commandP' = choose [loopP, funcP, cmdActP] <* optional (char ' ')

commandP :: Parser [Command]
commandP = many commandP' <* eof

strToCmd :: String -> Maybe [Command]
strToCmd str = case runParser commandP (map toLower str) of
  [] -> Nothing
  [(cs, _)] -> Just cs

expand :: Command -> [Action] -> [Action]
expand (Command a) _ = [a]
expand Function fx = fx
expand (Loop 0 a1 a2) _f = []
expand (Loop i a1 a2) _f = [a1, a2] ++ expand (Loop (i -1) a1 a2) _f

-- Expand a list of command to a list of actions (ie expand loop and function)
expandCmd :: [Command] -> [Action] -> [Action]
expandCmd cs fx = expandCmd' cs fx
  where
    expandCmd' [] _ = []
    expandCmd' (c : cs) fx = expand c fx ++ expandCmd' cs fx

test str = case strToCmd str of
    Nothing -> Nothing 
    Just cs -> Just (expandCmd cs [])
