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
import Database.Persist.MongoDB (MongoContext)

import Data.UUID (UUID)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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

data SandboxTransactionRegistry = Registry
  { transactions            :: Map UUID SandboxTransaction
  , participantTransactions :: Map (SessionId, ParticipantUid) SandboxTransaction
  }

empty :: SandboxTransactionRegistry
empty = Registry { transactions            = Map.empty
                 , participantTransactions = Map.empty
                 }

update :: SandboxTransaction
       -> SandboxTransactionRegistry
       -> SandboxTransactionRegistry
update transaction@SandboxTransaction{sessionId, participantId}
       registry@Registry{transactions, participantTransactions} =
  let remove = Map.delete . taskId
      insert = Map.insert (taskId transaction) transaction
      action = case Map.lookup (sessionId, participantId) participantTransactions of
                 Just prevTransaction -> insert . remove prevTransaction
                 Nothing              -> insert

  in registry { transactions            = action transactions
              , participantTransactions = Map.insert (sessionId, participantId)
                                                     transaction
                                                     participantTransactions
              }

get :: UUID -> SandboxTransactionRegistry -> Maybe SandboxTransaction
get transactionId = Map.lookup transactionId . transactions

delete :: UUID -> SandboxTransactionRegistry -> SandboxTransactionRegistry
delete transactionId registry@Registry{transactions, participantTransactions} =
  case Map.lookup transactionId transactions of
    Just SandboxTransaction{sessionId, participantId} ->
      registry { transactions            = Map.delete transactionId transactions
               , participantTransactions = Map.delete (sessionId, participantId) participantTransactions
               }
    Nothing -> registry
