-- |
-- Module      :  Util.hs
-- Description :  Collection of Helper functions
-- Copyright   :  Leung Chun Yin
-- License     :  MIT
--
-- Maintainer  :  kalvin80@hku.hk
-- Stability   :  experimental
-- Portability :  portable
--
-- Utils provide a collections of functions used in other module.
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

divToN :: Int -> [a] -> [[a]]
divToN n (c : cs)
  | length (c : cs) < n = []
  | otherwise = take n (c : cs) : divToN n cs

countSubArr :: Eq a => [[a]] -> [(Int, [a])]
countSubArr xss = [(count xs xss, xs) | xs <- xss]
  where
    count xs [] = 0
    count xs (xs' : xss) = oneIfEq xs xs' + count xs xss
    oneIfEq a b = if a == b then 1 else 0
