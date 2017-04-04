{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SequencePersistField where

import Database.Persist.Sql

import Data.Text (pack)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

instance PersistFieldSql a => PersistFieldSql (Seq a) where
  sqlType _ = SqlString

instance PersistField a => PersistField (Seq a) where
  toPersistValue = toPersistValue . toList
  fromPersistValue (PersistList list) = Seq.fromList <$> mapM fromPersistValue list
  fromPersistValue value = Left $ pack ("Failed to parse value" ++ show value)
