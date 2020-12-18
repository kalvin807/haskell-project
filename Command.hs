module Command where

import GameMap ( ColorTile )

data Direction = Up | Down | Left | Right deriving (Show)

data Action = Atom Direction | Condition ColorTile Direction deriving (Show)

allDirection :: [Direction]
allDirection = [Up, Down, Command.Left, Command.Right]
