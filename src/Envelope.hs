{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}

module Envelope where

import Control.Monad
import Data.Aeson
import Message

data Recipient = Messenger

instance Show Recipient where
  show Messenger = "messenger"

parseRecipient :: String -> Maybe Recipient
parseRecipient "messenger" = Just Messenger
parseRecipient _ = Nothing

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
  parseJSON (Object envelope) = do
    to <- envelope .: "to"
    case parseRecipient to of
      Just recipient -> Envelope <$> pure recipient <*> envelope .: "message"
      Nothing -> fail "Unknown recipient"
  parseJSON _ = mzero
