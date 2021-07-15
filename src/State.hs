{-# LANGUAGE NamedFieldPuns #-}

module State
  ( State(..)
  , SessionTimers(..)
  , empty
  , getSession
  , resetSyncSolutions
  , createSession
  , addSession
  , createTimers
  , stopTimers
  , getStartTimer
  , getRoundTimer
  , getSolutionSyncTimer
  , createSandboxTransaction
  , getSandboxTransaction
  , updateSandboxTransaction
  , clearSandboxTransactions
  , resolveSessionAlias
  , getSessionByAlias
  , addParticipant
  , removeParticipant
  , getPuzzleIndex
  , setPuzzleIndex
  , setRoundPhase
  , addRound
  , getStartCountdown
  , setStartCountdown
  , getRoundCountdown
  , setRoundCountdown
  , setParticipantInput
  , getSolutionTime
  , addSolution
  , hasCorrectSolution
  , getSandboxStatus
  , setSandboxStatus
  , getSessionForSandbox
  , syncSolutionsEmpty
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import           Data.Maybe (fromMaybe)
import           Control.Concurrent.Timer (TimerIO, newTimer, stopTimer)
import           Data.UUID (UUID)
import           Data.UUID.V4 (nextRandom)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime, NominalDiffTime)
import           Data.List (find)

import Participant ( Participant(Participant), ParticipantUid )
import Session
    ( Session(..),
      SessionAlias,
      SessionId )
import qualified Session
import Round (Round)
import RoundPhase ( RoundPhase(Idle) )
import Puzzle (Puzzle)
import SandboxTransaction (SandboxTransaction(SandboxTransaction),
                           SandboxTransactionRegistry,
                           update,
                           get
                           )
import qualified SandboxTransaction
import Solution (Solution(..))
import Game (Game)
import SandboxStatus (SandboxStatus(..))
import ServiceIdentity (ServiceIdentity, ServiceType(..))

data State = State
  { sessions :: Map SessionId Session
  , timers   :: Map SessionId SessionTimers
  , sandboxTransactions :: SandboxTransactionRegistry
  , aliases  :: Map (Game, SessionAlias) SessionId
  , identity :: Maybe ServiceIdentity
  }

data SessionTimers = SessionTimers
  { startTimer        :: TimerIO
  , roundTimer        :: TimerIO
  , solutionSyncTimer :: TimerIO
  }

empty :: State
empty = State
  { sessions            = Map.empty
  , timers              = Map.empty
  , sandboxTransactions = SandboxTransaction.empty
  , aliases             = Map.empty
  , identity            = Nothing
  }

createSession :: Game -> ParticipantUid -> SessionAlias -> [Puzzle] -> Int -> Session
createSession game gameMasterId alias puzzleList participantLimit = Session
  { game
  , gameMasterId
  , puzzles          = Seq.fromList puzzleList
  , participantLimit
  , sandboxStatus    = Requested
  , participants     = Map.empty
  , rounds           = Seq.empty
  , playerInput      = Map.empty
  , puzzleIndex      = Nothing
  , roundPhase       = Idle
  , startCountdown   = 0
  , roundCountdown   = 0
  , alias
  , syncSolutions    = Map.empty
  }

createTimers :: IO SessionTimers
createTimers = do
  startTmr <- newTimer
  roundTmr <- newTimer
  SessionTimers startTmr roundTmr <$> newTimer

createSandboxTransaction :: SessionId -> ParticipantUid -> Text -> UTCTime -> IO SandboxTransaction
createSandboxTransaction sessionId participantId input time = do
  taskId <- nextRandom
  return $ SandboxTransaction taskId sessionId participantId input time

updateSandboxTransaction :: SandboxTransaction -> State -> State
updateSandboxTransaction transaction state@State{sandboxTransactions} =
  state { sandboxTransactions = SandboxTransaction.update transaction sandboxTransactions }

getSandboxTransaction :: UUID -> State -> Maybe SandboxTransaction
getSandboxTransaction taskId State{sandboxTransactions} =
  SandboxTransaction.get taskId sandboxTransactions

clearSandboxTransactions :: State -> State
clearSandboxTransactions state = state { sandboxTransactions = SandboxTransaction.empty }

getStartTimer :: SessionId -> State -> Maybe TimerIO
getStartTimer sessionId State{timers} = startTimer <$> Map.lookup sessionId timers

getRoundTimer :: SessionId -> State -> Maybe TimerIO
getRoundTimer sessionId State{timers} = roundTimer <$> Map.lookup sessionId timers

getSolutionSyncTimer :: SessionId -> State -> Maybe TimerIO
getSolutionSyncTimer sessionId State{timers} = solutionSyncTimer <$> Map.lookup sessionId timers

stopTimers :: SessionId -> State -> IO ()
stopTimers sessionId state = do
  maybe (return ()) stopTimer (getStartTimer sessionId state)
  maybe (return ()) stopTimer (getSolutionSyncTimer sessionId state)
  maybe (return ()) stopTimer (getRoundTimer sessionId state)

addSession :: Session -> SessionId -> SessionTimers -> State -> State
addSession session@Session{alias, game} sessionId sessionTimers state@State{sessions, timers, aliases} =
  state { sessions = Map.insert sessionId session sessions
        , timers   = Map.insert sessionId sessionTimers timers
        , aliases  = Map.insert (game, alias) sessionId aliases
        }

getSession :: SessionId -> State -> Maybe Session
getSession sessionId State{sessions} = Map.lookup sessionId sessions

resolveSessionAlias :: Game -> SessionAlias -> State -> Maybe SessionId
resolveSessionAlias game alias State{aliases} = Map.lookup (game, alias) aliases

getSessionByAlias :: Game -> SessionAlias -> State -> Maybe (SessionId, Session)
getSessionByAlias game alias state = do
  sessionId <- resolveSessionAlias game alias state
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

setPuzzleIndex :: SessionId -> Maybe Int -> State -> State
setPuzzleIndex sessionId index state@State{sessions} =
  state { sessions = Map.adjust modify sessionId sessions }
    where modify = Session.setPuzzleIndex index

getPuzzleIndex :: SessionId -> State -> Maybe Int
getPuzzleIndex sessionId State{sessions} = do
  session <- Map.lookup sessionId sessions
  Session.puzzleIndex session

setRoundPhase :: SessionId -> RoundPhase -> State -> State
setRoundPhase sessionId phase state@State{sessions} =
  state { sessions = Map.adjust modify sessionId sessions }
    where modify = Session.setRoundPhase phase

addRound :: SessionId -> Round -> State -> State
addRound sessionId newRound state@State{sessions} =
  state { sessions = Map.adjust modify sessionId sessions }
    where modify = Session.addRound newRound

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

hasCorrectSolution :: SessionId -> ParticipantUid -> State -> Bool
hasCorrectSolution sessionId participantId state = fromMaybe False $ do
  session <- getSession sessionId state
  return $ Session.hasCorrectSolution participantId session

getSandboxStatus :: SessionId -> State -> Maybe SandboxStatus
getSandboxStatus sessionId State{sessions} = do
  session <- Map.lookup sessionId sessions
  return $ Session.sandboxStatus session

setSandboxStatus :: SessionId -> SandboxStatus -> State -> State
setSandboxStatus sessionId sandboxStatus state@State{sessions} =
  state { sessions = Map.adjust modify sessionId sessions }
    where modify = Session.setSandboxStatus sandboxStatus

getSessionForSandbox :: State -> ServiceType -> Maybe SessionId
getSessionForSandbox State{sessions} (SandboxService sandboxGame) =
  fst <$> find (\(_, Session{game, sandboxStatus}) -> game == sandboxGame && sandboxStatus == Requested) (Map.toList sessions)
getSessionForSandbox _ _ = Nothing

syncSolutionsEmpty :: SessionId -> State -> Bool
syncSolutionsEmpty sessionId State{sessions} = fromMaybe False $ do
  session <- Map.lookup sessionId sessions
  return $ Map.null $ syncSolutions session

resetSyncSolutions :: SessionId -> State -> State
resetSyncSolutions sessionId state@State{sessions} =
  state { sessions = Map.adjust modify sessionId sessions }
    where modify session = session { syncSolutions = Map.empty }
