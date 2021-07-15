{-# LANGUAGE TemplateHaskell #-}

module SandboxStatus
  ( SandboxStatus(..)
  , toSimpleJSON
  ) where

import Database.Persist.TH ( derivePersistField )

import Data.Aeson
    ( (.:),
      object,
      FromJSON(parseJSON),
      Value(Object, String),
      KeyValue((.=)),
      ToJSON(toJSON) )
import Control.Monad (mzero)
import ServiceIdentity (ServiceIdentity)

data SandboxStatus
  = Requested
  | Ready ServiceIdentity
  deriving stock (Show, Read, Eq)
derivePersistField "SandboxStatus"

instance ToJSON SandboxStatus where
  toJSON Requested        = String "requested"
  toJSON (Ready identity) = object [ "identity" .= identity ]

instance FromJSON SandboxStatus where
  parseJSON (String "requested") = return Requested
  parseJSON (Object obj)         = Ready <$> obj .: "identity"
  parseJSON _ = mzero

toSimpleJSON :: SandboxStatus -> Value
toSimpleJSON Requested = String "requested"
toSimpleJSON (Ready _) = String "ready"
