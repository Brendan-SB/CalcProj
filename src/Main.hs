module Main where

import Bread (finder, formatter)

main :: IO ()
main = do
  content <- finder
  bread <- formatter content
  return ()
