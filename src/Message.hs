{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Message where

import Prelude hiding (id)

import Data.Text
import Data.Text.Encoding
import Data.Aeson
import Data.Semigroup

import State
import Participant

data IncomingMessage
  = CreateSession Participant

data OutgoingMessage
  = ArnauxCheckin String

toName :: OutgoingMessage -> String
toName (ArnauxCheckin _) = "checkin"

instance ToJSON OutgoingMessage where
  toJSON message = object $ ["name" .= toName message] <> toValue message
    where
      toValue (ArnauxCheckin identity) = ["identity" .= identity]

instance FromJSON IncomingMessage where
  parseJSON (Object message) = do
    name <- message .: "name"
    case name of
      String "session.create" -> CreateSession <$> message .: "gameMaster"
