{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module State where

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import           Data.Maybe (fromMaybe)
import           Control.Concurrent.Timer
import           Data.UUID (UUID)
import           Data.UUID.V4 (nextRandom)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime, NominalDiffTime)
import           Data.ByteString.Lazy (ByteString)

import Participant
import Session
import Round (Round)
import RoundPhase
import Puzzle
import SandboxTransaction (SandboxTransaction(SandboxTransaction), taskId)
import Solution (Solution)

data State = State
  { sessions :: Map SessionId Session
  , timers   :: Map SessionId SessionTimers
  , sandboxTransactions :: Map UUID SandboxTransaction
  , aliases :: Map SessionAlias SessionId
  }

data SessionTimers = SessionTimers
  { startTimer :: TimerIO
  , roundTimer :: TimerIO
  }

empty :: State
empty = State
  { sessions = Map.empty
  , timers = Map.empty
  , sandboxTransactions = Map.empty
  , aliases = Map.empty
  }

createSession :: ParticipantUid -> SessionAlias -> [Puzzle] -> Session
createSession gameMasterId alias puzzleList = Session
  { gameMasterId
  , puzzles        = Seq.fromList puzzleList
  , participants   = Map.empty
  , rounds         = Seq.empty
  , playerInput    = Map.empty
  , puzzleIndex    = 0
  , roundPhase     = Idle
  , startCountdown = 0
  , roundCountdown = 0
  , alias
  }

createTimers :: IO SessionTimers
createTimers = do
  startTmr <- newTimer
  roundTmr <- newTimer
  return $ SessionTimers startTmr roundTmr

createSandboxTransaction ::SessionId -> SessionAlias -> ParticipantUid -> Text -> UTCTime -> IO SandboxTransaction
createSandboxTransaction sessionId sessionAlias participantId input time =
  SandboxTransaction
    <$> nextRandom
    <*> pure sessionId
    <*> pure sessionAlias
    <*> pure participantId
    <*> pure input
    <*> pure time

addSandboxTransaction :: SandboxTransaction -> State -> State
addSandboxTransaction transaction@SandboxTransaction{taskId} state@State{sandboxTransactions} =
  state { sandboxTransactions = Map.insert taskId transaction sandboxTransactions }

getSandboxTransaction :: UUID -> State -> Maybe SandboxTransaction
getSandboxTransaction taskId State{sandboxTransactions} = Map.lookup taskId sandboxTransactions

removeSandboxTransaction :: UUID -> State -> State
removeSandboxTransaction taskId state@State{sandboxTransactions} =
  state { sandboxTransactions = Map.delete taskId sandboxTransactions }

clearSandboxTransactions :: State -> State
clearSandboxTransactions state = state { sandboxTransactions = Map.empty }

getStartTimer :: SessionId -> State -> Maybe TimerIO
getStartTimer sessionId State{timers} = startTimer <$> Map.lookup sessionId timers

getRoundTimer :: SessionId -> State -> Maybe TimerIO
getRoundTimer sessionId State{timers} = roundTimer <$> Map.lookup sessionId timers

stopTimers :: SessionId -> State -> IO ()
stopTimers sessionId state = do
  fromMaybe (return ()) $ stopTimer <$> getStartTimer sessionId state
  fromMaybe (return ()) $ stopTimer <$> getRoundTimer sessionId state

addSession :: Session -> SessionId -> SessionTimers -> State -> State
addSession session@Session{alias} sessionId sessionTimers state@State{sessions, timers, aliases} =
  state { sessions = Map.insert sessionId session sessions
        , timers   = Map.insert sessionId sessionTimers timers
        , aliases  = Map.insert alias sessionId aliases
        }

getSession :: SessionId -> State -> Maybe Session
getSession sessionId State{sessions} = Map.lookup sessionId sessions

resolveSessionAlias :: SessionAlias -> State -> Maybe SessionId
resolveSessionAlias alias State{aliases} = Map.lookup alias aliases

getSessionByAlias :: SessionAlias -> State -> Maybe (SessionId, Session)
getSessionByAlias alias state = do
  sessionId <- resolveSessionAlias alias state
  session <- getSession sessionId state
  return (sessionId, session)

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

setParticipantInput :: SessionId -> ParticipantUid -> Text -> State -> State
setParticipantInput sessionId participantId input state@State{sessions} =
  state { sessions = Map.adjust modify sessionId sessions }
    where modify = Session.setParticipantInput participantId input

isSolutionCorrect :: SessionId -> ByteString -> State -> Bool
isSolutionCorrect sessionId json state =
  case getSession sessionId state of
    Just session ->
      Session.isSolutionCorrect json session
    Nothing -> False

getSolutionTime :: SessionId -> UTCTime -> State -> NominalDiffTime
getSolutionTime sessionId time state =
  case getSession sessionId state of
    Just session ->
      Session.getSolutionTime time session
    Nothing -> 0

addSolution :: SessionId -> ParticipantUid -> Solution -> State -> State
addSolution sessionId participantId solution state@State{sessions} =
  state { sessions = Map.adjust modify sessionId sessions }
    where modify = Session.addSolution participantId solution
