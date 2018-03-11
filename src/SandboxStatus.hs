{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module SandboxStatus where

import Database.Persist.TH

import Data.Aeson
import Control.Monad (mzero)

data SandboxStatus
  = Requested
  | Ready
  deriving (Show, Read, Eq)
derivePersistField "SandboxStatus"

instance ToJSON SandboxStatus where
  toJSON Requested = String "requested"
  toJSON Ready     = String "ready"

instance FromJSON SandboxStatus where
  parseJSON (String "requested") = return Requested
  parseJSON (String "ready")     = return Ready
  parseJSON _ = mzero
