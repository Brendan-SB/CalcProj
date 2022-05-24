module Main where

import Bread (Bread, breadToMD, finder, formatter, title)
import System.IO

writeBread :: Bread -> IO ()
writeBread bread = do
  let path = ("recipes/" ++ title bread ++ ".md")
  putStrLn $ "Formatting to markdown and writing to " ++ path
  writeFile path $ breadToMD bread

main :: IO ()
main = do
  content <- finder
  bread <- formatter content
  mapM writeBread bread
  return ()
