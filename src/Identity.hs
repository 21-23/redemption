{-# LANGUAGE OverloadedStrings #-}

module Identity where

import Data.Text (pack)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson

data Identity
  = StateService
  | Messenger
  | FrontService
  | SandboxService
  | InitService

instance Show Identity where
  show StateService   = "state-service"
  show Messenger      = "messenger"
  show FrontService   = "front-service"
  show SandboxService = "sandbox-service"
  show InitService    = "init-service"

instance ToJSON Identity where
  toJSON = Aeson.String . pack . show

parseIdentity :: String -> Maybe Identity
parseIdentity "state-service"   = Just StateService
parseIdentity "messenger"       = Just Messenger
parseIdentity "front-service"   = Just FrontService
parseIdentity "sandbox-service" = Just SandboxService
parseIdentity "init-service"    = Just InitService
parseIdentity _                 = Nothing
