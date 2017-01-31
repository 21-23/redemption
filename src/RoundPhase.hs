{-# LANGUAGE OverloadedStrings #-}

module RoundPhase where

import Prelude hiding (String)
import Control.Monad
import Data.Text (pack, toLower)
import Data.Aeson (ToJSON, FromJSON, Value(String))
import qualified Data.Aeson as Aeson

data RoundPhase
  = Idle
  | Countdown
  | Game
  | End
  deriving (Show)

instance ToJSON RoundPhase where
  toJSON = String . toLower . pack . show

instance FromJSON RoundPhase where
  parseJSON (String "idle")      = return Idle
  parseJSON (String "countdown") = return Countdown
  parseJSON (String "game")      = return Game
  parseJSON (String "end")       = return End
  parseJSON _ = mzero
