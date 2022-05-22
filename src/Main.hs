module Main where

import Bread (finder, formatter)
import System.IO

main :: IO ()
main = do
  content <- finder
  formatted <- formatter content
  return ()
