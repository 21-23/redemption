{-# OPTIONS_GHC -fno-warn-orphans #-}

module UUIDPersistField () where

import Database.Persist.Sql
    ( PersistValue(PersistText),
      PersistFieldSql(..),
      SqlType(SqlString),
      PersistField(..) )

import Data.Text (pack)
import Data.UUID ( UUID, fromText, toText )

instance PersistFieldSql UUID where
  sqlType _ = SqlString

instance PersistField UUID where
  toPersistValue = toPersistValue . toText
  fromPersistValue (PersistText text) = maybe errorValue Right $ fromText text
    where errorValue = Left $ pack ("Failed to parse value" ++ show text)
  fromPersistValue value = Left $ pack ("Failed to parse value" ++ show value)
