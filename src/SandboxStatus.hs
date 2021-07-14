{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module SandboxStatus where

import Database.Persist.TH

import Data.Aeson
import Control.Monad (mzero)
import ServiceIdentity (ServiceIdentity)

data SandboxStatus
  = Requested
  | Ready ServiceIdentity
  deriving (Show, Read, Eq)
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
