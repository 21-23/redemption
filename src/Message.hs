{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Message where

import Control.Monad
import Data.Aeson
import Data.Semigroup

import Identity
import Participant
import Session

data IncomingMessage
  = CreateSession Participant

data OutgoingMessage
  = ArnauxCheckin Identity
  | SessionCreated Session

toName :: OutgoingMessage -> String
toName (ArnauxCheckin _) = "checkin"
toName (SessionCreated _) = "session.created"

instance ToJSON OutgoingMessage where
  toJSON message = object $ ["name" .= toName message] <> toValue message
    where
      toValue (ArnauxCheckin identity) = ["identity" .= identity]
      toValue (SessionCreated session) = ["session" .= session]

instance FromJSON IncomingMessage where
  parseJSON (Object message) = do
    name <- message .: "name"
    case name of
      String "session.create" -> CreateSession <$> message .: "gameMaster"
      _ -> fail "Unrecognized incoming message"
  parseJSON _ = mzero
