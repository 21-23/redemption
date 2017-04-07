{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module State where

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
-- import Data.Time.Clock
import           Control.Concurrent.Timer

import Participant
import Session
import Round (Round)
import RoundPhase
-- import Solution
import Puzzle

data State = State
  { sessions :: Map SessionId Session
  , timers   :: Map SessionId SessionTimers
  }

data SessionTimers = SessionTimers
  { startTimer :: TimerIO
  , roundTimer :: TimerIO
  }

empty :: State
empty = State { sessions = Map.empty, timers = Map.empty }

createSession :: ParticipantUid -> [Puzzle] -> Session
createSession gameMasterId puzzleList = Session
  { gameMaster     = gameMasterId
  , puzzles        = Seq.fromList puzzleList
  , participants   = Map.empty
  , rounds         = Seq.empty
  , playerInput    = Map.empty
  , puzzleIndex    = 0
  , roundPhase     = Idle
  , startCountdown = 0
  , roundCountdown = 0
  }

createTimers :: IO SessionTimers
createTimers = do
  startTmr <- newTimer
  roundTmr <- newTimer
  return $ SessionTimers startTmr roundTmr

getStartTimer :: SessionId -> State -> Maybe TimerIO
getStartTimer sessionId State{timers} = startTimer <$> Map.lookup sessionId timers

getRoundTimer :: SessionId -> State -> Maybe TimerIO
getRoundTimer sessionId State{timers} = roundTimer <$> Map.lookup sessionId timers

addSession :: Session -> SessionId -> SessionTimers -> State -> State
addSession session sessionId sessionTimers state@State{sessions,timers} =
  state { sessions = Map.insert sessionId session sessions
        , timers = Map.insert sessionId sessionTimers timers
        }

getSession :: SessionId -> State -> Maybe Session
getSession sessionId State{sessions} = Map.lookup sessionId sessions

addParticipant :: SessionId -> ParticipantUid -> State -> State
addParticipant sessionId participantId state@State{sessions} =
  state { sessions = Map.adjust add sessionId sessions }
    where add = Session.addParticipant $ Participant participantId

removeParticipant :: SessionId -> ParticipantUid -> State -> State
removeParticipant sessionId participantId state@State{sessions} =
  state { sessions = Map.adjust remove sessionId sessions }
    where remove = Session.removeParticipant participantId

setPuzzleIndex :: SessionId -> Int -> State -> State
setPuzzleIndex sessionId index state@State{sessions} =
  state { sessions = Map.adjust modify sessionId sessions }
    where modify = Session.setPuzzleIndex index

getPuzzleIndex :: SessionId -> State -> Maybe Int
getPuzzleIndex sessionId State{sessions} = do
  session <- Map.lookup sessionId sessions
  return $ Session.puzzleIndex session

setRoundPhase :: SessionId -> RoundPhase -> State -> State
setRoundPhase sessionId phase state@State{sessions} =
  state { sessions = Map.adjust modify sessionId sessions }
    where modify = Session.setRoundPhase phase

addRound :: SessionId -> Round -> State -> State
addRound sessionId newRound state@State{sessions} =
  state { sessions = Map.adjust modify sessionId sessions }
    where modify = Session.addRound newRound
--
setStartCountdown :: SessionId -> Int -> State -> State
setStartCountdown sessionId value state@State{sessions} =
  state { sessions = Map.adjust modify sessionId sessions }
    where modify = Session.setStartCountdown value

getStartCountdown :: SessionId -> State -> Maybe Int
getStartCountdown sessionId State{sessions} = do
  session <- Map.lookup sessionId sessions
  return $ Session.getStartCountdown session

setRoundCountdown :: SessionId -> Int -> State -> State
setRoundCountdown sessionId value state@State{sessions} =
  state { sessions = Map.adjust modify sessionId sessions }
    where modify = Session.setRoundCountdown value

getRoundCountdown :: SessionId -> State -> Maybe Int
getRoundCountdown sessionId State{sessions} = do
  session <- Map.lookup sessionId sessions
  return $ Session.roundCountdown session
--
-- setParticipantInput :: SessionId -> ParticipantRef -> String -> State -> State
-- setParticipantInput sessionId participantId input state@State{sessions} =
--   state { sessions = Map.adjust modify sessionId sessions }
--     where modify = Session.setParticipantInput participantId input
--
-- addSolution :: SessionId -> ParticipantRef -> Solution -> State -> State
-- addSolution sessionId participantId solution state@State{sessions} =
--   state { sessions = Map.adjust modify sessionId sessions }
--     where modify = Session.addSolution participantId solution
--
-- getLastRoundScore :: SessionId -> State -> Map ParticipantRef NominalDiffTime
-- getLastRoundScore sessionId State{sessions} =
--   case Map.lookup sessionId sessions of
--     Just session -> Session.getLastRoundScore session
--     Nothing -> Map.empty
