{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NominalDiffTimePersistField where

import Database.Persist.Sql
import Data.Time.Clock (NominalDiffTime)
import Data.Text (pack)

instance PersistFieldSql NominalDiffTime where
  sqlType _ = SqlNumeric 32 20

instance PersistField NominalDiffTime where
  toPersistValue = PersistRational . toRational
  fromPersistValue (PersistRational rational) = Right $ fromRational rational
  fromPersistValue value = Left $ pack ("Failed to parse value" ++ show value)
