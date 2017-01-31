{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module State where

import Data.Map (Map)
import qualified Data.Map as Map

import Participant
import Session
import Reference
import RoundPhase

data State = State { sessions :: Map SessionRef Session }

empty :: State
empty = State { sessions = Map.empty }

createSession :: SessionRef -> Participant -> Session
createSession sid gameMaster = Session
  { sessionId = sid
  , gameMaster
  , participants = Map.empty
  , puzzles = []
  , puzzleIndex = 0
  , roundPhase = Idle
  }

addSession :: Session -> State -> State
addSession session@Session{sessionId} state@State{sessions} =
  state { sessions = Map.insert sessionId session sessions }

addParticipant :: SessionRef -> Participant -> State -> State
addParticipant sessionId participant state@State{sessions} =
  state { sessions = Map.adjust add sessionId sessions }
    where add = Session.addParticipant participant

removeParticipant :: SessionRef -> ParticipantRef -> State -> State
removeParticipant sessionId participantId state@State{sessions} =
  state { sessions = Map.adjust remove sessionId sessions }
    where remove = Session.removeParticipant participantId

setPuzzleIndex :: SessionRef -> Int -> State -> State
setPuzzleIndex sessionId index state@State{sessions} =
  state { sessions = Map.adjust modify sessionId sessions }
    where modify = Session.setPuzzleIndex index

setRoundPhase :: SessionRef -> RoundPhase -> State -> State
setRoundPhase sessionId phase state@State{sessions} =
  state { sessions = Map.adjust modify sessionId sessions }
    where modify = Session.setRoundPhase phase
