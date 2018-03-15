{-# LANGUAGE OverloadedStrings #-}

module ServiceIdentity where

import           Data.Text  (Text, pack, unpack, splitOn)
import           Data.Semigroup ((<>))
import           Data.Aeson (ToJSON(toJSON), FromJSON, Value(String, Object), object, (.=), (.:))
import qualified Data.Aeson  as Aeson
import           Control.Monad (MonadPlus, mzero)
import           Data.UUID (UUID)

import           Game (Game, stringify, parseGame)

data ServiceType
  = StateService
  | FrontService
  | SandboxService Game
  | InitService
  | ContainerService
  deriving (Eq, Ord, Read)

instance Show ServiceType where
  show StateService          = "state-service"
  show FrontService          = "front-service"
  show (SandboxService game) = "sandbox-service" <> ":" <> unpack (stringify game)
  show InitService           = "init-service"
  show ContainerService      = "container-service"

instance ToJSON ServiceType where
  toJSON = Aeson.String . pack . show

instance FromJSON ServiceType where
  parseJSON (String serviceType) = parseServiceType serviceType
  parseJSON serviceType          = fail $ "Unrecognized service type: " <> show serviceType

parseServiceType :: (MonadPlus m) => Text -> m ServiceType
parseServiceType "state-service"     = pure StateService
parseServiceType "front-service"     = pure FrontService
parseServiceType "init-service"      = pure InitService
parseServiceType "container-service" = pure ContainerService
parseServiceType string =
  case splitOn ":" string of
    ["sandbox-service", gameType]   -> SandboxService <$> parseGame gameType
    _                               -> mzero

data ServiceIdentity = ServiceIdentity ServiceType UUID
  deriving (Eq, Ord, Read, Show)

instance ToJSON ServiceIdentity where
  toJSON (ServiceIdentity serviceType uuid) = object
    [ "type" .= serviceType
    , "id"   .= uuid
    ]

instance FromJSON ServiceIdentity where
  parseJSON (Object serviceIdentity) =
    ServiceIdentity
      <$> serviceIdentity .: "type"
      <*> serviceIdentity .: "id"
  parseJSON serviceIdentity = fail $ "Bad service identity: " <> show serviceIdentity

data ServiceSelector
  = Messenger
  | Service ServiceIdentity
  | AnyOfType ServiceType
  deriving (Eq, Show)

instance ToJSON ServiceSelector where
  toJSON Messenger = String "messenger"
  toJSON (Service serviceIdentity) = toJSON serviceIdentity
  toJSON (AnyOfType serviceType) = toJSON serviceType

instance FromJSON ServiceSelector where
  parseJSON (String "messenger") = pure Messenger
  parseJSON (String string)      = AnyOfType <$> parseServiceType string
  parseJSON (Object selector)    = do
    serviceIdentity <- ServiceIdentity
                         <$> selector .: "type"
                         <*> selector .: "id"
    pure $ Service serviceIdentity
  parseJSON serviceSelector      = fail $ "Bad service selector: " <> show serviceSelector
