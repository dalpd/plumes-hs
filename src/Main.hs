module Main where

import ParseDat

main :: IO ()
main = do
  contents <- lines <$> readFile "test"
  mapM_ (run parsePlumes) contents
  
