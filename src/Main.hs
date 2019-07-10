module Main where

import ParseDat

getSimData :: [String] -> [String]
getSimData [] = []
getSimData (x:xs) =
  case x of
    "Simulation:" -> drop 3 xs
    _ -> getSimData xs

main :: IO ()
main = do
  contents <- getSimData <$> lines <$> readFile "test"
  mapM_ (run parsePlumes) contents
  
