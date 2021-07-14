{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module SolutionCorrectness where

import Database.Persist.TH

import Data.Aeson
import Control.Monad (mzero)

data SolutionCorrectness
  = Incorrect
  | Partial
  | Correct
  deriving (Show, Read, Eq)
derivePersistField "SolutionCorrectness"

instance ToJSON SolutionCorrectness where
  toJSON Correct   = String "correct"
  toJSON Partial   = String "partial"
  toJSON Incorrect = String "incorrect"

instance FromJSON SolutionCorrectness where
  parseJSON (String "correct")   = return Correct
  parseJSON (String "partial")   = return Partial
  parseJSON (String "incorrect") = return Incorrect
  parseJSON _ = mzero
