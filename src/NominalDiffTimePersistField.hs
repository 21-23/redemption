{-# OPTIONS_GHC -fno-warn-orphans #-}

module NominalDiffTimePersistField () where

import           Data.Convertible.Base           (convert)
import           Data.Convertible.Instances.Time ()
import           Data.Text                       (pack)
import           Data.Time.Clock                 (NominalDiffTime)
import           Database.Persist.Sql            (PersistField (..),
                                                  PersistFieldSql (..),
                                                  PersistValue (PersistInt64),
                                                  SqlType (SqlReal))

instance PersistFieldSql NominalDiffTime where
  sqlType _ = SqlReal

instance PersistField NominalDiffTime where
  toPersistValue = PersistInt64 . fromInteger . convert

  fromPersistValue (PersistInt64 value) = Right $ convert $ toInteger value
  fromPersistValue value = Left $ pack ("Failed to parse value" ++ show value)
