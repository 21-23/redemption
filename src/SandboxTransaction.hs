{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module SandboxTransaction where

import Language.Haskell.TH.Syntax
import Database.Persist.TH
import Database.Persist.MongoDB

import Data.UUID (UUID)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import UUIDPersistField()
import Session (SessionId)
import Participant (ParticipantUid)

let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) { mpsGeneric = False, mpsPrefixFields = False }
 in share [mkPersist mongoSettings] [persistLowerCase|
SandboxTransaction json
  taskId        UUID
  sessionId     SessionId
  participantId ParticipantUid
  input         Text
  time          UTCTime
|]
