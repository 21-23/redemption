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
  | Game
  | End
  deriving (Eq, Show, Read)
derivePersistField "RoundPhase"

instance ToJSON RoundPhase where
  toJSON = String . toLower . pack . show

instance FromJSON RoundPhase where
  parseJSON (String "idle")      = return Idle
  parseJSON (String "countdown") = return Countdown
  parseJSON (String "game")      = return Game
  parseJSON (String "end")       = return End
  parseJSON _ = mzero
