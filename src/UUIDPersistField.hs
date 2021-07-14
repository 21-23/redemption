{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UUIDPersistField where

import Database.Persist.Sql

import Data.Text (pack)
import Data.UUID

instance PersistFieldSql UUID where
  sqlType _ = SqlString

instance PersistField UUID where
  toPersistValue = toPersistValue . toText
  fromPersistValue (PersistText text) = maybe errorValue Right $ fromText text
    where errorValue = Left $ pack ("Failed to parse value" ++ show text)
  fromPersistValue value = Left $ pack ("Failed to parse value" ++ show value)
