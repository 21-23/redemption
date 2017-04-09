{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UUIDAeson where

import Data.Aeson
import Control.Monad (mzero)
import Data.UUID

instance ToJSON UUID where
  toJSON = String . toText

instance FromJSON UUID where
  parseJSON (String text) = maybe mzero return $ fromText text
  parseJSON _ = mzero
