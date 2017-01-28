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

data Message
  = ArnauxCheckin String
  | CreateSession Participant

toName :: Message -> String
toName (ArnauxCheckin _) = "checkin"
toName (CreateSession _) = "session.create"

instance ToJSON Message where
  toJSON message = object $ ["name" .= toName message] <> toValue message
    where
      toValue (ArnauxCheckin identity) = ["identity" .= identity]
      toValue (CreateSession gameMaster) = ["gameMaster" .= toJSON gameMaster]

instance FromJSON Message where
  parseJSON (Object message) = do
    name <- message .: "name"
    case name of
      String "checkin" -> ArnauxCheckin <$> message .: "identity"
      String "session.create" -> CreateSession <$> message .: "gameMaster"
