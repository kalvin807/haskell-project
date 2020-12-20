module Util where

import System.IO

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

cliColor :: String -> String
cliColor tile = "\27[" ++ show (color tile) ++ "m" ++ tile
  where
    color "*" = 37
    color "@" = 91
    color "-" = 30
    color "t" = 91
    color "y" = 93
    color "g" = 92
    color "p" = 95
    color "b" = 96
    color _ = 0
