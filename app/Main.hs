{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Concurrent
import           Control.Monad          (forever)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Network                (PortID(PortNumber))
import           Network.Socket         (withSocketsDo)
import qualified Network.WebSockets     as WS
import           Database.Persist
import           Database.Persist.MongoDB
import           Data.Aeson
import           Control.Concurrent.Timer
import           Control.Concurrent.Suspend.Lifted
import           Data.Time.Clock        (getCurrentTime)
import qualified Data.Map               as Map
import qualified Data.Yaml              as Yaml
import           System.Environment
import           Data.Maybe
import           Control.Exception
import           Data.Convertible.Base
import           Data.Convertible.Instances.Time()
import qualified Data.Text              as Text


import Config
import State (State)
import qualified State
import Message
import Identity
import Envelope
import Session (EntityField(..), SessionAlias)
import qualified Session as Session
import Puzzle (EntityField(..), timeLimit)
import Round (Round(..))
import RoundPhase
import SandboxTransaction (SandboxTransaction(..))
import Solution (Solution(Solution))
import qualified Role

updateState :: MVar State -> (State -> State) -> IO ()
updateState stateVar action = do
  state <- takeMVar stateVar
  putMVar stateVar $ action state

sendMessage :: WS.Connection -> Identity -> OutgoingMessage -> IO ()
sendMessage connection to message =
  WS.sendTextData connection $ encode $ Envelope to message

stopRound :: MVar State -> ConnectionPool -> WS.Connection -> SessionAlias -> IO ()
stopRound stateVar pool connection sessionAlias = do
  let mongo = runMongoDBAction pool
  state <- readMVar stateVar
  case State.getSessionByAlias sessionAlias state of
    Just (sessionId, session) -> do
      updateState stateVar $ (State.clearSandboxTransactions . State.setRoundPhase sessionId End)
      mongo $ update sessionId [RoundPhase =. End]
      sendMessage connection SandboxService ResetSandbox
      sendMessage connection FrontService $ RoundPhaseChanged sessionAlias End
      sendMessage connection FrontService $ Score sessionAlias $ Session.getPlayerRoundData session
    Nothing -> putStrLn $ "Session not found: " ++ show sessionAlias

startCountdownAction :: TimerIO -> MVar State -> ConnectionPool -> SessionAlias -> WS.Connection -> IO ()
startCountdownAction timer stateVar pool sessionAlias connection = do
  let mongo = runMongoDBAction pool
  state <- readMVar stateVar
  case State.getSessionByAlias sessionAlias state of
    Nothing -> do
      putStrLn $ "Session not found: " ++ show sessionAlias
    Just (sessionId, session) -> do
      let countdownValue = State.getStartCountdown sessionId state
      case countdownValue of
        Just 0 ->
          case State.getPuzzleIndex sessionId state of
            Just puzzleIndex -> do
              -- create a new round
              currentTime <- getCurrentTime
              updateState stateVar $ State.addRound sessionId $ Round puzzleIndex currentTime Map.empty
              mongo $ update sessionId [Rounds =. Session.rounds session]
              -- change phase to 'game'
              updateState stateVar $ State.setRoundPhase sessionId InProgress
              mongo $ update sessionId [RoundPhase =. InProgress]
              sendMessage connection FrontService $ RoundPhaseChanged sessionAlias InProgress

              let maybePuzzle = State.getSession sessionId state >>= Session.lookupPuzzle puzzleIndex
              case maybePuzzle of
                Just puzzle -> do
                  -- send round puzzle
                  sendMessage connection FrontService $ RoundPuzzle sessionAlias puzzle
                  -- start round countdown
                  let roundCountdownValue = convert $ timeLimit puzzle
                  updateState stateVar $ State.setRoundCountdown sessionId roundCountdownValue
                  mongo $ update sessionId [RoundCountdown =. roundCountdownValue]
                  case State.getRoundTimer sessionId state of
                    Just rountTimer -> do
                      _ <- repeatedStart rountTimer (roundCountdownAction rountTimer stateVar pool sessionAlias connection) (sDelay 1)
                      return ()
                    Nothing -> putStrLn $ "Timer error for session " ++ show sessionId
                  stopTimer timer -- has to be the last statement because it kills the thread
                Nothing -> do
                  putStrLn $ "Puzzle not found: index " ++ show puzzleIndex
                  stopTimer timer -- has to be the last statement because it kills the thread

              stopTimer timer -- has to be the last statement because it kills the thread

            Nothing -> stopTimer timer -- has to be the last statement because it kills the thread
        Just value -> do
          let nextValue = value - 1
          updateState stateVar $ State.setStartCountdown sessionId nextValue
          sendMessage connection FrontService $ StartCountdownChanged sessionAlias nextValue
        Nothing -> return ()

roundCountdownAction :: TimerIO -> MVar State -> ConnectionPool -> SessionAlias -> WS.Connection -> IO ()
roundCountdownAction timer stateVar pool sessionAlias connection = do
  let mongo = runMongoDBAction pool
  state <- readMVar stateVar
  case State.getSessionByAlias sessionAlias state of
    Just (sessionId, _) -> do
      let countdownValue = State.getRoundCountdown sessionId state
      case countdownValue of
        Just 0 -> do
          stopRound stateVar pool connection sessionAlias
          stopTimer timer -- has to be the last statement because it kills the thread

        Just value -> do
          let nextValue = value - 1
          updateState stateVar $ State.setRoundCountdown sessionId nextValue
          mongo $ update sessionId [RoundCountdown =. nextValue]
          sendMessage connection FrontService $ RoundCountdownChanged sessionAlias nextValue

        Nothing -> return ()
    Nothing -> putStrLn $ "Session not found: " ++ show sessionAlias

app :: MVar State -> ConnectionPool -> WS.ClientApp ()
app stateVar pool connection = do
  WS.sendTextData connection $ encode $ Envelope Messenger (ArnauxCheckin StateService)
  let mongo = runMongoDBAction pool
  forever $ do
    string <- WS.receiveData connection
    case eitherDecode string :: Either String (Envelope IncomingMessage) of
      Right Envelope {message} -> case message of

        CreatePuzzle puzzle -> do
          puzzleId <- mongo $ insert puzzle
          sendMessage connection InitService $ PuzzleCreated puzzleId

        CreateSession gameMasterId sessionAlias puzzleIds -> do
          puzzles <- mongo $ selectList [PuzzleId <-. puzzleIds] []
          let session = State.createSession gameMasterId sessionAlias (entityVal <$> puzzles)
          sessionId <- mongo $ insert session
          mongo $ repsert sessionId session
          sessionTimers <- State.createTimers
          updateState stateVar $ State.addSession session sessionId sessionTimers
          sendMessage connection InitService $ SessionCreated sessionAlias

        JoinSession sessionAlias participantId -> do
          state <- readMVar stateVar
          let stateSession = State.getSessionByAlias sessionAlias state
          maybeSession <- case stateSession of
            Just s -> pure $ Just s
            Nothing -> do
              dbSession <- mongo $ selectFirst [Alias ==. sessionAlias] []
              case dbSession of
                Just entity -> do
                  let sessionId = entityKey entity
                      session = entityVal entity
                  sessionTimers <- State.createTimers
                  updateState stateVar $ State.addSession session sessionId sessionTimers
                  return $ Just (sessionId, session)
                Nothing -> return Nothing

          case maybeSession of
            Just (sessionId, session) -> do
              updateState stateVar $ State.addParticipant sessionId participantId
              mongo $ update sessionId [Participants =. Session.participants session]
              let role = Session.getParticipantRole participantId session
                  participantData = ParticipantJoined sessionAlias participantId role
              sendMessage connection FrontService participantData
              let sessionStateMessage = case role of
                                          Role.Player -> PlayerSessionState
                                          Role.GameMaster -> GameMasterSessionState
              sendMessage connection FrontService $ sessionStateMessage sessionAlias participantId session
            Nothing -> putStrLn $ "Couldn't resolve session alias: " ++ show sessionAlias

        LeaveSession sessionAlias participantId -> do
          state <- readMVar stateVar
          case State.getSessionByAlias sessionAlias state of
            Just (sessionId, session) -> do
              updateState stateVar $ State.removeParticipant sessionId participantId
              mongo $ update sessionId [Participants =. Session.participants session]
              sendMessage connection FrontService $ ParticipantLeft sessionAlias participantId
            Nothing -> putStrLn $ "Session not found: " ++ show sessionAlias

        SetPuzzleIndex sessionAlias puzzleIndex -> do
          state <- readMVar stateVar
          case State.getSessionByAlias sessionAlias state of
            Just (sessionId, session) -> do
              updateState stateVar $ State.setPuzzleIndex sessionId puzzleIndex
              mongo $ update sessionId [PuzzleIndex =. Session.puzzleIndex session]
              case Session.lookupPuzzle puzzleIndex session of
                Just puzzle -> do
                  sendMessage connection FrontService $ PuzzleChanged sessionAlias puzzleIndex puzzle
                Nothing -> putStrLn $ "Puzzle not found: index " ++ show puzzleIndex
            Nothing -> putStrLn $ "Session not found: " ++ show sessionAlias

        StartRound sessionAlias -> do
          state <- readMVar stateVar
          case State.getSessionByAlias sessionAlias state of
            Just (sessionId, session) -> do
              updateState stateVar $ State.setRoundPhase sessionId Countdown
              mongo $ update sessionId [RoundPhase =. Countdown]
              let puzzleIndex = State.getPuzzleIndex sessionId state
              let maybePuzzle = puzzleIndex >>= (flip Session.lookupPuzzle) session
              case maybePuzzle of
                Just puzzle -> do
                  sendMessage connection SandboxService $ SetSandbox puzzle
                  sendMessage connection FrontService $ RoundPhaseChanged sessionAlias Countdown
                  let countdownValue = 2
                  updateState stateVar $ State.setStartCountdown sessionId countdownValue
                  mongo $ update sessionId [StartCountdown =. countdownValue]
                  case State.getStartTimer sessionId state of
                    Just timer -> do
                      _ <- repeatedStart timer (startCountdownAction timer stateVar pool sessionAlias connection) (sDelay 1)
                      sendMessage connection FrontService $ StartCountdownChanged sessionAlias countdownValue
                    Nothing -> putStrLn $ "Timer error for session " ++ show sessionId
                Nothing -> putStrLn $ "Puzzle not found: index " ++ show puzzleIndex
            Nothing -> putStrLn $ "Session not found: " ++ show sessionAlias

        StopRound sessionAlias -> do
          state <- readMVar stateVar
          case State.getSessionByAlias sessionAlias state of
            Just (sessionId, _) -> do
              State.stopTimers sessionId state
              stopRound stateVar pool connection sessionAlias
            Nothing -> putStrLn $ "Session not found: " ++ show sessionAlias

        ParticipantInput sessionAlias participantId input timestamp -> do
          state <- readMVar stateVar
          case State.getSessionByAlias sessionAlias state of
            Just (sessionId, _) -> do
              -- this update is not synced with the database, participant input is considered perishable
              updateState stateVar $ State.setParticipantInput sessionId participantId input
              transaction <- State.createSandboxTransaction sessionId sessionAlias participantId input timestamp
              updateState stateVar $ State.addSandboxTransaction transaction
              sendMessage connection SandboxService $ EvaluateSolution (taskId transaction) input
            Nothing -> putStrLn $ "Session not found: " ++ show sessionAlias

        EvaluatedSolution taskId result -> do
          state <- readMVar stateVar
          case State.getSandboxTransaction taskId state of
            Just SandboxTransaction{sessionId, sessionAlias, participantId, input, time} -> do
              let solutionTime = State.getSolutionTime sessionId time state
              let solutionLength = Text.length input
                  correct = case result of
                              Left _ -> False
                              Right resultJson -> State.isSolutionCorrect sessionId resultJson state
              updateState stateVar $ State.addSolution sessionId participantId $ Solution input solutionTime correct
              sendMessage connection FrontService $ SolutionEvaluated
                                                      sessionAlias
                                                      participantId
                                                      result
                                                      solutionTime
                                                      solutionLength
                                                      correct

            Nothing -> putStrLn $ "Transaction not found: " ++ show taskId


      Left err -> putStrLn err

connectToMessenger :: Config -> WS.ClientApp () -> IO ()
connectToMessenger config@Config{messengerHost, messengerPort} clientApp =
  catch
    (withSocketsDo $ WS.runClient messengerHost messengerPort "/" clientApp)
    (\exception -> do
      print (exception :: IOException)
      suspend $ msDelay 500
      connectToMessenger config clientApp)

runMongoDBAction :: (MonadIO m, MonadBaseControl IO m) => ConnectionPool -> Action m a -> m a
runMongoDBAction = flip $ runMongoDBPool master

main :: IO ()
main = do
  maybeEnv <- lookupEnv "redemption_environment"
  let env = fromMaybe "dev" maybeEnv
  maybeConfig <- Yaml.decodeFile ("conf/" ++ env ++ ".yaml")
  case maybeConfig of
    Just config -> do
      stateVar <- newMVar State.empty
      withMongoDBPool "_qd" (Config.mongoDBHost config) (PortNumber 27017) Nothing 2 1 20000 $ \pool ->
        connectToMessenger config $ app stateVar pool
    Nothing -> fail ("Configuration file '" ++ env ++ "' was not found")
