{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module State where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Participant
import Session
import Reference
import Round (Round)
import RoundPhase
import Solution
import Data.Time.Clock

data State = State { sessions :: Map SessionRef Session }

empty :: State
empty = State { sessions = Map.empty }

createSession :: SessionRef -> Participant -> Session
createSession sid gameMaster = Session
  { sessionId      = sid
  , gameMaster
  , puzzles        = []
  , participants   = Map.empty
  , rounds         = Seq.empty
  , input          = Map.empty
  , puzzleIndex    = 0
  , roundPhase     = Idle
  , startCountdown = 0
  , roundCountdown = 0
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

getPuzzleIndex :: SessionRef -> State -> Maybe Int
getPuzzleIndex sessionId State{sessions} = do
  session <- Map.lookup sessionId sessions
  return $ Session.getPuzzleIndex session

setRoundPhase :: SessionRef -> RoundPhase -> State -> State
setRoundPhase sessionId phase state@State{sessions} =
  state { sessions = Map.adjust modify sessionId sessions }
    where modify = Session.setRoundPhase phase

addRound :: SessionRef -> Round -> State -> State
addRound sessionId newRound state@State{sessions} =
  state { sessions = Map.adjust modify sessionId sessions }
    where modify = Session.addRound newRound

setStartCountdown :: SessionRef -> Int -> State -> State
setStartCountdown sessionId value state@State{sessions} =
  state { sessions = Map.adjust modify sessionId sessions }
    where modify = Session.setStartCountdown value

getStartCountdown :: SessionRef -> State -> Maybe Int
getStartCountdown sessionId State{sessions} = do
  session <- Map.lookup sessionId sessions
  return $ Session.getStartCountdown session

setRoundCountdown :: SessionRef -> Int -> State -> State
setRoundCountdown sessionId value state@State{sessions} =
  state { sessions = Map.adjust modify sessionId sessions }
    where modify = Session.setRoundCountdown value

getRoundCountdown :: SessionRef -> State -> Maybe Int
getRoundCountdown sessionId State{sessions} = do
  session <- Map.lookup sessionId sessions
  return $ Session.getRoundCountdown session

setParticipantInput :: SessionRef -> ParticipantRef -> String -> State -> State
setParticipantInput sessionId participantId input state@State{sessions} =
  state { sessions = Map.adjust modify sessionId sessions }
    where modify = Session.setParticipantInput participantId input

addSolution :: SessionRef -> ParticipantRef -> Solution -> State -> State
addSolution sessionId participantId solution state@State{sessions} =
  state { sessions = Map.adjust modify sessionId sessions }
    where modify = Session.addSolution participantId solution

getLastRoundScore :: SessionRef -> State -> Map ParticipantRef NominalDiffTime
getLastRoundScore sessionId State{sessions} =
  case Map.lookup sessionId sessions of
    Just session -> Session.getLastRoundScore session
    Nothing -> Map.empty
