{-# LANGUAGE TemplateHaskell #-}

module RoundPhase
  ( RoundPhase (..)
  ) where

import Control.Monad ( MonadPlus(mzero) )
import Data.Text (pack, toLower)

import Data.Aeson
    ( FromJSON(parseJSON), Value(String), ToJSON(toJSON) )
import Database.Persist.TH ( derivePersistField )

data RoundPhase
  = Idle
  | Countdown
  | InProgress
  | End
  deriving stock (Eq, Show, Read)

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
