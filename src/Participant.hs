{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Participant where

import Control.Monad
import Data.Aeson
import Database.MongoDB

import BSON

data Participant = Participant
  { participantId :: String
  , name :: String
  }

instance ToJSON Participant where
  toJSON Participant {participantId, name} =
    object [
      "id" .= participantId,
      "name" .= name
      ]

instance FromJSON Participant where
  parseJSON (Object participant) = Participant
    <$> participant .: "id"
    <*> participant .: "name"
  parseJSON _ = mzero

instance ToBSON Participant where
  toBSON Participant {participantId, name} = [ "_id" =: participantId, "name" =: name ]
