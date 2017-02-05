{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Monad

import Data.Yaml

data Config = Config
  { mongoDBHost   :: String
  , messengerHost :: String
  , messengerPort :: Int
  }

instance FromJSON Config where
  parseJSON (Object config) = do
    mongoDBSettings <- config .: "mongodb"
    messengerSettings <- config .: "messenger"
    Config
      <$> mongoDBSettings .: "host"
      <*> messengerSettings .: "host"
      <*> messengerSettings .: "port"
  parseJSON _ = mzero
