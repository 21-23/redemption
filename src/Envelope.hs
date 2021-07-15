module Envelope (Envelope(..)) where

import Data.Aeson      (Value(Object),
                        FromJSON(parseJSON),
                        ToJSON(toJSON), (.:), (.=), object)
import ServiceIdentity (ServiceSelector)

data Envelope a = Envelope
  { to      :: ServiceSelector
  , message :: a
  }

instance (ToJSON msg) => ToJSON (Envelope msg) where
  toJSON (Envelope to message) = object
    [ "to"      .= to
    , "message" .= message
    ]

instance (FromJSON msg) => FromJSON (Envelope msg) where
  parseJSON (Object envelope) =
    Envelope
      <$> envelope .: "to"
      <*> envelope .: "message"
  parseJSON _ = fail "Bad message envelope format"
