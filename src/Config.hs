module Config
  ( Config(..)
  ) where

import Control.Monad ( MonadPlus(mzero) )

import Data.Yaml ( (.:), FromJSON(parseJSON), Value(Object) )

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
