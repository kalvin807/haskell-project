-- |
-- Module      :  Command.hs
-- Description :  Command parser to read/validate input
-- Copyright   :  Leung Chun Yin
-- License     :  MIT
--
-- Maintainer  :  kalvin80@hku.hk
-- Stability   :  experimental
-- Portability :  portable
--
-- Command module provides a parser to read and validate input string and translate into a sequence of command.
module Command where

import Control.Applicative (Alternative (many), optional)
import Control.Monad
import qualified Data.Bifunctor
import Data.Char (digitToInt, toLower)
import Data.List
import GameMap (ColorTile (..), Coord)
import Parser
  ( Parser,
    char,
    choose,
    eof,
    many,
    runParser,
    satisfy,
    string,
  )
import Util

data Direction = CUp | CDown | CLeft | CRight deriving (Eq)

data Action = Action Direction | Condition ColorTile Direction deriving (Eq)

data Command = Command Action | Loop Int Action Action | Function deriving (Eq)

instance Show Direction where
  show CUp = "Up"
  show CDown = "Down"
  show CLeft = "Left"
  show CRight = "Right"

instance Show Action where
  show (Action dir) = show dir
  show (Condition clr dir) = "Cond{" ++ show clr ++ "}{" ++ show dir ++ "}"

instance Show Command where
  show (Command a) = show a
  show (Loop i a1 a2) = "Loop{" ++ show i ++ "}{" ++ show a1 ++ "," ++ show a2 ++ "}"
  show Function = "Function"

allDirection :: [Direction]
allDirection = [CUp, CDown, CLeft, CRight]

colorP :: Parser ColorTile
colorP = choose [pinkP, yellowP, greenP]
  where
    pinkP = char 'p' >> return Pink
    yellowP = char 'y' >> return Yellow
    greenP = char 'g' >> return Green

dirP :: Parser Direction
dirP = choose [upP, downP, leftP, rightP]
  where
    upP = string "up" >> return CUp
    downP = string "down" >> return CDown
    leftP = string "left" >> return CLeft
    rightP = string "right" >> return CRight

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

commandP :: Parser [Command]
commandP = many commandP' <* eof
  where
    commandP' = choose [loopP, funcP, cmdActP] <* optional (char ' ')

funcCommandP :: Parser [Command]
funcCommandP = many funcCommandP' <* eof
  where
    funcCommandP' = cmdActP <* optional (char ' ')

strToCmd :: Parser [Command] -> String -> Maybe [Command]
strToCmd p str = case runParser p (map toLower str) of
  [] -> Nothing
  [(cs, _)] -> Just cs

parseCommand :: String -> Maybe [Command]
parseCommand = strToCmd commandP

parseFunction :: String -> Maybe [Command]
parseFunction = strToCmd funcCommandP

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

-- Helper function that match directions to their coordinate change
getNextPos :: Direction -> Coord -> Coord
getNextPos CUp (r, c) = (r - 1, c)
getNextPos CDown (r, c) = (r + 1, c)
getNextPos CLeft (r, c) = (r, c - 1)
getNextPos CRight (r, c) = (r, c + 1)

-- Concat actions that is size of 3 and repeated 1 more time
concatFunc :: [Command] -> ([Command], [Command])
concatFunc cs
  | length cs < 3 = (cs, [])
  | otherwise =
    let subArrs = noLoop $ divToN 3 cs
        counter = countSubArr subArrs
     in case max' counter (head counter) of
          (1, arr) -> (cs, [])
          (_, arr) -> (replaceFunc arr cs, arr)
  where
    replaceFunc func [] = []
    replaceFunc func (c : cs)
      | length (c : cs) < 3 = c : cs
      | take 3 (c : cs) == func = Function : replaceFunc func (drop 3 (c : cs))
      | otherwise = c : replaceFunc func (drop 1 (c : cs))
    max' [] ma = ma
    max' ((n, nA) : xs) (m, mA)
      | n > m = max' xs (n, nA)
      | otherwise = max' xs (m, mA)
    noLoop [] = []
    noLoop (xs : xss)
      | any isLoop xs = noLoop xss
      | otherwise = xs : xss
    isLoop Loop {} = True
    isLoop _ = False

-- Concat actions that is size of 2 nd repeated consecutively >= 2
concatLoop :: [Command] -> [Command]
concatLoop cmds
  | length cmds < 4 = cmds
  | otherwise = concatLoop' cmds
  where
    concatLoop' cs
      | length cs < 4 = cs
    concatLoop' (c0 : c1 : c2 : c3 : rem) =
      if c0 == c2 && c1 == c3
        then
          let (n, rem') = countLoop rem c0 c1 2
           in makeLoop c0 c1 n : concatLoop' rem'
        else c0 : concatLoop' (c1 : c2 : c3 : rem)
    countLoop cs' a b n
      | length cs' < 2 = (n, cs')
      | n == 5 = (5, cs')
      | otherwise =
        let (a' : b' : re) = cs'
         in if a == a' && b == b'
              then countLoop re a b (n + 1)
              else (n, cs')
    makeLoop (Command a) (Command b) n = Loop n a b

-- Merge extra conditional action that is same as last direction
mergeDir :: [Command] -> [Command]
mergeDir [] = []
mergeDir [c] = [c]
mergeDir (c : cs) = case c of
  Command (Action dir) -> c : mergeDir (tryMergeNext cs dir)
  _ -> c : mergeDir cs
  where
    tryMergeNext ((Command (Condition c dir')) : cs) dir
      | dir' == dir = cs
      | otherwise = Command (Condition c dir') : cs
    tryMergeNext cs dir = cs

-- reduce a set of cmd to a set of cmd uses loop and function
concatCmd :: [Command] -> ([Command], [Command])
concatCmd = concatFunc . concatLoop . mergeDir
