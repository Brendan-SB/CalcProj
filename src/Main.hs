module Main where

import Bread (finder, formatter)
import System.IO

main :: IO ()
main = do
  content <- readFile "test.html"
  formatted <- formatter content
  return ()
