{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module RoundPhase where

import Control.Monad
import Data.Text (pack, toLower)

import Data.Aeson
import Database.Persist.TH

data RoundPhase
  = Idle
  | Countdown
  | InProgress
  | End
  deriving (Eq, Show, Read)
derivePersistField "RoundPhase"

instance ToJSON RoundPhase where
  toJSON InProgress = String "in-progress" -- this one just had to have a hyphen, hadn't it
  toJSON phase = String . toLower . pack . show $ phase

instance FromJSON RoundPhase where
  parseJSON (String "idle")        = return Idle
  parseJSON (String "countdown")   = return Countdown
  parseJSON (String "in-progress") = return InProgress
  parseJSON (String "end")         = return End
  parseJSON _ = mzero
