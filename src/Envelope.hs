{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}

module Envelope where

import Data.Aeson
import Message

data Recipient = Messenger

instance Show Recipient where
  show Messenger = "messenger"

parseRecipient :: String -> Recipient
parseRecipient "messenger" = Messenger

data Envelope a = Envelope
  { to :: Recipient
  , message :: a
  }

instance ToJSON (Envelope OutgoingMessage) where
  toJSON Envelope { to, message } = object [
    "to" .= show to,
    "message" .= toJSON message
    ]

instance FromJSON (Envelope IncomingMessage) where
  parseJSON (Object envelope) = Envelope
    <$> (parseRecipient <$> envelope .: "to")
    <*> envelope .: "message"
