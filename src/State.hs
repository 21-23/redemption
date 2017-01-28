{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module State where

import Data.Map (Map)
import qualified Data.Map as Map
import Database.MongoDB

import Participant
import Session

data State = State { sessions :: Map ObjectId Session }

empty :: State
empty = State { sessions = Map.empty }

createSession :: ObjectId -> Participant -> Session
createSession oid gameMaster =
  Session { sessionId = oid, gameMaster, participants = Map.empty }

addSession :: Session -> State -> State
addSession session@Session{sessionId} state@State{sessions} =
  state { sessions = Map.insert sessionId session sessions }
