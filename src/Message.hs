{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Message where

import Data.Text
import Data.Text.Encoding
import Data.Aeson
import Data.Semigroup

data Message =
  ArnauxCheckin String

toName :: Message -> String
toName (ArnauxCheckin _) = "checkin"

instance ToJSON Message where
  toJSON message = object $ ["name" .= toName message] <> toValue message
    where
      toValue (ArnauxCheckin identity) = ["identity" .= identity]

instance FromJSON Message where
  parseJSON = withObject "message" $ \object -> do
    name <- object .: "name"
    case name of
      String "checkin" -> ArnauxCheckin <$> object .: "identity"

data Recipient = Messenger

instance Show Recipient where
  show Messenger = "messenger"

parseRecipient :: String -> Recipient
parseRecipient "messenger" = Messenger

data Envelope = Envelope
  { to :: Recipient
  , message :: Message
  }

instance ToJSON Envelope where
  toJSON Envelope { to, message } = object [
    "to" .= show to,
    "message" .= toJSON message
    ]

instance FromJSON Envelope where
  parseJSON = withObject "envelope" $ \object -> Envelope
    <$> (parseRecipient <$> object .: "to")
    <*> object .: "message"
