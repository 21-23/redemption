{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NominalDiffTimePersistField where

import Database.Persist.Sql
import Data.Time.Clock (NominalDiffTime)
import Data.Text (pack)
import Data.Ratio

instance PersistFieldSql NominalDiffTime where
  sqlType _ = SqlReal

instance PersistField NominalDiffTime where
  toPersistValue timeDiff =
    let rational = toRational timeDiff
        num = PersistInt64 $ fromInteger $ numerator rational
        den = PersistInt64 $ fromInteger $ denominator rational
    in PersistList [num, den]

  fromPersistValue (PersistList [PersistInt64 num, PersistInt64 den]) = Right $ fromRational $ toInteger num % toInteger den
  fromPersistValue value = Left $ pack ("Failed to parse value" ++ show value)
