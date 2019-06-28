{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE FlexibleContexts #-}

module ParseDat where

import Control.Applicative
-- import Control.Monad.Identity (Identity)

import Text.Parsec.String (Parser)
import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
-- import Text.Parsec.Token as ParsecToken

data SimState = StreamLimit 
              | BottomHit 
              | BeginOverlap 
              | LocalMaximum 
              | EndOverlap 
              | StopDilution
              | DefaultState
              | UnknownState Error
              deriving Show

type Error = String

data SimColumns = SimColumns
    { _step    :: Double
    , _depth   :: Double
    , _amb_cur :: Double
    , _far_dir :: Double
    , _disprsn :: Double
    , _p_dia   :: Double
    , _v_angle :: Double
    , _p_depth :: Double
    , _polutnt :: Double
    , _dilutn  :: Double
    , _cl_dlin :: Double
    , _x_posn  :: Double
    , _y_posn  :: Double
    , _state   :: SimState
    } deriving Show

matchState :: String -> SimState
matchState = \case
  "stream limit reached"  -> StreamLimit
  "bottom hit"            -> BottomHit
  "begin overlap"         -> BeginOverlap
  "local maximum rise or fall" -> LocalMaximum
  "end overlap"           -> EndOverlap
  "stop dilution reached" -> StopDilution
  _                       -> UnknownState "ERROR"

run :: Show a => Parser a -> String -> IO ()
run p str =
    case Parsec.parse p "" str of
      Left err  -> do putStrLn "Parser Error"
                      print err
      Right res -> print res
      
parseNum :: Parser Double
parseNum =
      Parsec.try (read <$> parseScientific)
  <|> Parsec.try (read <$> parseFloat)
  <|> Parsec.try (read <$> parseInt)    

parsePositive :: Parsec.Parsec String () String
parsePositive = do
  s <- Parsec.many1 Parsec.digit
  return s

parseNegative :: Parsec.Parsec String () String
parseNegative = do
  Parsec.char '-'
  s <- parsePositive
  return $ "-" ++ s

parseInt :: Parsec.Parsec String () String
parseInt = Parsec.try parsePositive <|> parseNegative

parseFloat :: Parsec.Parsec String () String
parseFloat = do
   digits <- Parsec.many1 Parsec.digit
   Parsec.char '.'
   digits' <- Parsec.many1 Parsec.digit
   return $ digits ++ "." ++ digits'

parseScientific :: Parsec.Parsec String () String
parseScientific = do
  digits <- parseFloat <|> parseInt
  Parsec.oneOf "eE"
  Parsec.char '+'
  digits' <- parseInt
  return $ digits ++ "E" ++ digits'

--parseMessage :: Parsec.Parsec String () SimState
parseMessage :: Parsec.Parsec String () String
parseMessage = undefined
  
parsePlumes :: Parsec.Parsec String () SimColumns
parsePlumes = do
  Parsec.spaces
  step <- parseNum
  Parsec.spaces
  depth <- parseNum
  Parsec.spaces
  amb_cur <- parseNum
  Parsec.spaces
  far_dir <- parseNum
  Parsec.spaces
  disprsn <- parseNum
  Parsec.spaces
  p_dia <- parseNum
  Parsec.spaces
  v_angle <- parseNum
  Parsec.spaces
  p_depth <- parseNum
  Parsec.spaces
  polutnt <- parseNum
  Parsec.spaces
  dilutn <- parseNum
  Parsec.spaces
  cl_diln <- parseNum
  Parsec.spaces
  x_posn <- parseNum
  Parsec.spaces
  y_posn <- parseNum
  return $
    SimColumns step depth amb_cur far_dir disprsn p_dia
               v_angle p_depth polutnt dilutn cl_diln x_posn y_posn (matchState "")


