{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module SandboxSettings where

import Control.Monad
import Data.Time.Clock (NominalDiffTime)
import Data.Aeson
import Database.MongoDB

import BSON

data SandboxSettings = SandboxSettings
  { timeout       :: NominalDiffTime
  , reloadWorkers :: Bool
  , refillWorkers :: Bool
  }

instance FromJSON SandboxSettings where
  parseJSON (Object settings) = SandboxSettings
    <$> settings .: "timeout"
    <*> settings .: "reloadWorkers"
    <*> settings .: "refillWorkers"
  parseJSON _ = mzero

instance ToJSON SandboxSettings where
  toJSON SandboxSettings{timeout, reloadWorkers, refillWorkers} = object
    [ "timeout" .= timeout
    , "reloadWorkers" .= reloadWorkers
    , "refillWorkers" .= refillWorkers
    ]

instance ToBSON SandboxSettings where
  toBSON SandboxSettings{timeout, reloadWorkers, refillWorkers} =
    [ "timeout" =: timeout
    , "reloadWorkers" =: reloadWorkers
    , "refillWorkers" =: refillWorkers
    ]
