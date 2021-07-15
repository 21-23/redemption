{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module PuzzleOptions
  ( PuzzleOptions(..)
  , toSimpleJSON
  ) where

import Language.Haskell.TH.Syntax (Type(..))
import Database.Persist.TH
    ( mkPersist,
      mkPersistSettings,
      persistLowerCase,
      share,
      MkPersistSettings(mpsPrefixFields) )
import Database.Persist.MongoDB
    ( BackendKey(MongoKey), MongoContext )

import Data.Time.Clock (NominalDiffTime)
import Data.Aeson (Value, object, (.=))

import NominalDiffTimePersistField()

let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) { mpsPrefixFields = False }
 in share [mkPersist mongoSettings] [persistLowerCase|
PuzzleOptions json
  timeLimit         NominalDiffTime
  bannedCharacters [String] Maybe
|]

toSimpleJSON :: PuzzleOptions -> Value
toSimpleJSON PuzzleOptions { timeLimit, bannedCharacters } =
  object (["timeLimit" .= timeLimit] <> bannedJSONValue)
    where
      bannedJSONValue = case bannedCharacters of
                          Just characterList -> [ "bannedCharacters" .= characterList ]
                          Nothing            -> []
