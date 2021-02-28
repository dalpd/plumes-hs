module Main
  ( main
  )
where

-- import ParseDat
import qualified Data.Text as T
import Parser
import Text.Megaparsec (runParser)

getSimData :: [String] -> [String]
getSimData [] = []
getSimData (x:xs) =
  case x of
    "Simulation:" -> drop 3 xs
    _ -> getSimData xs

main :: IO ()
main = do
  -- contents <- getSimData <$> lines <$> readFile "test"
  contents <- lines <$> readFile "test"
  let res = runParser parseHeader "test" (T.pack $ concat contents)
  print res
  -- mapM_ (run parsePlumes) contents
  
