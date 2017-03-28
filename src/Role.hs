{-# LANGUAGE OverloadedStrings #-}

module Role where

import Control.Monad

import Data.Aeson

data Role
  = GameMaster
  | Player

instance ToJSON Role where
  toJSON GameMaster = String "game-master"
  toJSON Player     = String "player"

instance FromJSON Role where
  parseJSON (String "game-master") = return GameMaster
  parseJSON (String "player")      = return Player
  parseJSON _ = mzero
