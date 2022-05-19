module Main where

import Bread (finder)

main :: IO ()
main = do
  info <- finder

  putStrLn "Saving bread..."

  writeFile "test.html" info 

  putStrLn "Bread has been saved."


