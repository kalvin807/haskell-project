module Command where

data Direction = Up | Down | Left | Right deriving (Show)

data Color = Pink | Blue

data Action = Direction | Condition Color Direction
