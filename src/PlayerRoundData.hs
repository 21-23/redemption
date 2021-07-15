{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module PlayerRoundData
  ( PlayerRoundData (..)
  ) where

import Language.Haskell.TH.Syntax ( Type(ConT) )
import Database.Persist.TH
    ( mkPersist,
      mkPersistSettings,
      persistLowerCase,
      share,
      MkPersistSettings(mpsPrefixFields) )
import Database.Persist.MongoDB
    ( BackendKey(MongoKey), MongoContext )

import Data.Time.Clock ( NominalDiffTime )

import Participant (ParticipantUid)
import Solution (Solution)
import NominalDiffTimePersistField()

let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) { mpsPrefixFields = False }
 in share [mkPersist mongoSettings] [persistLowerCase|
PlayerRoundData json
  participantId ParticipantUid
  inputLength Int
  solution Solution Maybe
  aggregateScore NominalDiffTime Maybe
|]
