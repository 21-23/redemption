{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}

module Envelope where

import Control.Monad
import Data.Aeson
import Message
import Identity

data Envelope a = Envelope
  { to :: Identity
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
    case parseIdentity to of
      Just identity -> Envelope <$> pure identity <*> envelope .: "message"
      Nothing -> fail "Unknown identity"
  parseJSON _ = mzero
