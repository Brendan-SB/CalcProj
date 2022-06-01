module Main where

import Bread (finder, formatter)

main :: IO ()
main = do
  putStrLn "Initializing bread sequence"
  content <- finder
  _ <- formatter content
  putStrLn "Bread has been downloaded"
