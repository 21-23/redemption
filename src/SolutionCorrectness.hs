{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module SolutionCorrectness
  ( SolutionCorrectness(..)
  ) where

import Database.Persist.TH ( derivePersistField )

import Data.Aeson
    ( FromJSON(parseJSON), Value(String), ToJSON(toJSON) )
import Control.Monad (mzero)

data SolutionCorrectness
  = Incorrect
  | Partial
  | Correct
  deriving stock (Show, Read, Eq)
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
