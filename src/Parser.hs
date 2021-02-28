{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
-- |
module Parser
  ( SimHeader (..),
    SimError (..),
    SimModel (..),
    parseHeader,
  )
where

------------------------------------------------------------------------------

import Text.Megaparsec (Parsec, Token, (<|>))
import Text.Megaparsec.Char (char, space, string)
import Data.Text (Text, null)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay)
import Numeric.Natural (Natural)
import Path (Abs, File, Path)
import Relude hiding (null)

-- import 
------------------------------------------------------------------------------

-- |
data SimError
  = SimError_CatchAll
  deriving stock (Eq, Show)

deriving instance Ord SimError
  
data SimModel
  = SimModel_UM3
  | SimModel_Unknown
  deriving stock Show

------------------------------------------------------------------------------
-- |

type Parser a = Parsec SimError Text a

data SimHeader = SimHeader
  { _simHeader_model :: SimModel
    --_simHeader_day :: Day,
    --_simHeader_time :: TimeOfDay,
    --_simHeader_caseNumber :: Natural,    
    --_simHeader_ambientFilePath :: Path Abs File,
    --_simHeader_diffuserRecordNumber :: Natural
  }
  deriving stock Show
------------------------------------------------------------------------------

-- | The first two lines of the export, contains general information about the
-- following:
-- * Simulation model
-- * Date and time
-- * Case number
-- * Ambient file path
-- * Diffuser table record number
parseHeader :: Parser SimHeader
parseHeader = do
  _simHeader_model <- parseModel
  -- _simHeader_day <- undefined -- parseDay
  -- _simHeader_time <- parseTime
  -- _simHeader_caseNumber <- parseCase
  -- _simHeader_ambientFilePath <- undefined -- parseAbsFile
  -- _simHeader_diffuserRecordNumber <- parseRecord
  pure $
    SimHeader {..}

------------------------------------------------------------------------------

-- | Model used while creating a simulation.
--
-- The  UM3  model  is  based  on  the  projected  area  entrainment
-- hypothesis,  which  assumes  ambient fluid is entrained into the plume
-- through areas projected in directions along the plume centerline and
-- perpendicular to the centerline (US EPA, 1994).
parseModel :: Parser SimModel
parseModel = do
  _ <- char '/'
  _ <- space
  model <- string "UM3" <|> string "Windows UM3"
  _ <- char '.'
  pure $ case null model of
    True -> SimModel_Unknown
    False -> SimModel_UM3
    
-- 
-- ------------------------------------------------------------------------------
-- parseDate = do
--   undefined
-- 
-- ------------------------------------------------------------------------------
-- parseTime = do
--   undefined
-- 
-- ------------------------------------------------------------------------------
-- parseCase = do
--   undefined
-- 
-- ------------------------------------------------------------------------------
-- parsePath = do
--   undefined
-- 
-- ------------------------------------------------------------------------------
-- parseRecord = do
--     undefined
-- 
