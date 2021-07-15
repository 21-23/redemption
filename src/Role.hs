module Role
  ( Role(..)
  ) where

import           Control.Monad (MonadPlus (mzero))

import           Data.Aeson    (FromJSON (parseJSON), ToJSON (toJSON),
                                Value (String))

data Role
  = GameMaster
  | Player deriving stock Eq

instance ToJSON Role where
  toJSON GameMaster = String "game-master"
  toJSON Player     = String "player"

instance FromJSON Role where
  parseJSON (String "game-master") = pure GameMaster
  parseJSON (String "player")      = pure Player
  parseJSON _                      = mzero
