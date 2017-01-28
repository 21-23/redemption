{-# LANGUAGE OverloadedStrings #-}

module Identity where

import Data.Text (pack)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson

data Identity
  = StateService
  | Messenger
  | FrontService

instance Show Identity where
  show StateService = "state-service"
  show Messenger    = "messenger"
  show FrontService = "front-service"

instance ToJSON Identity where
  toJSON = Aeson.String . pack . show

parseIdentity :: String -> Maybe Identity
parseIdentity "state-service" = Just StateService
parseIdentity "messenger"     = Just Messenger
parseIdentity "front-service" = Just FrontService
parseIdentity _               = Nothing
