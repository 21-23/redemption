{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Envelope where

import Data.Aeson
import Message

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
  parseJSON (Object envelope) = Envelope
    <$> (parseRecipient <$> envelope .: "to")
    <*> envelope .: "message"
