{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NominalDiffTimePersistField where

import Database.Persist.Sql
import Data.Time.Clock (NominalDiffTime)
import Data.Text (pack)
import Data.Convertible.Base
import Data.Convertible.Instances.Time()

instance PersistFieldSql NominalDiffTime where
  sqlType _ = SqlReal

instance PersistField NominalDiffTime where
  toPersistValue timeDiff = PersistInt64 $ fromInteger $ convert timeDiff

  fromPersistValue (PersistInt64 value) = Right $ convert $ toInteger value
  fromPersistValue value = Left $ pack ("Failed to parse value" ++ show value)
