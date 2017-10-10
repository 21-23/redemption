{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Concurrent
import           Control.Monad          (forever, unless)
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
import qualified Data.Map.Strict        as Map
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
import Session (EntityField(..), Session, SessionId)
import qualified Session
import Puzzle (EntityField(PuzzleId), options)
import PuzzleOptions (timeLimit)
import Round (Round(..))
import RoundPhase
import SandboxTransaction (SandboxTransaction(..))
import Solution (Solution(Solution))
import qualified Role

updateState :: MVar State -> (State -> State) -> IO ()
updateState stateVar action = do
  state <- takeMVar stateVar
  putMVar stateVar $ action state

withSession :: MVar State -> SessionId -> (Session -> IO ()) -> IO ()
withSession stateVar sessionId action = do
  state <- readMVar stateVar
  case State.getSession sessionId state of
    Just session -> action session
    Nothing -> return ()

sendMessage :: WS.Connection -> Identity -> OutgoingMessage -> IO ()
sendMessage connection to message =
  WS.sendTextData connection $ encode $ Envelope to message

stopRound :: MVar State -> ConnectionPool -> WS.Connection -> SessionId -> IO ()
stopRound stateVar pool connection sessionId = do
  let mongo = runMongoDBAction pool
  state <- readMVar stateVar
  case State.getSession sessionId state of
    Just session -> do
      updateState stateVar (State.clearSandboxTransactions . State.setRoundPhase sessionId End)
      mongo $ update sessionId [RoundPhase =. End, Rounds =. Session.rounds session]
      sendMessage connection SandboxService ResetSandbox
      sendMessage connection FrontService $ RoundPhaseChanged sessionId End
      sendMessage connection FrontService $ Score sessionId $ Session.getPlayerRoundData session
    Nothing -> putStrLn $ "Session not found: " ++ show sessionId

startCountdownAction :: TimerIO -> MVar State -> ConnectionPool -> SessionId -> WS.Connection -> IO ()
startCountdownAction timer stateVar pool sessionId connection = do
  let mongo = runMongoDBAction pool
  state <- readMVar stateVar
  case State.getSession sessionId state of
    Nothing -> putStrLn $ "Session not found: " ++ show sessionId
    Just _ -> do
      let countdownValue = State.getStartCountdown sessionId state
      case countdownValue of
        Just 0 ->
          case State.getPuzzleIndex sessionId state of
            Just puzzleIndex -> do
              -- create a new round
              currentTime <- getCurrentTime
              updateState stateVar $ State.addRound sessionId $ Round puzzleIndex currentTime Map.empty
              withSession stateVar sessionId $ \session ->
                mongo $ update sessionId [Rounds =. Session.rounds session]
              -- change phase to 'in progress'
              updateState stateVar $ State.setRoundPhase sessionId InProgress
              mongo $ update sessionId [RoundPhase =. InProgress]
              sendMessage connection FrontService $ RoundPhaseChanged sessionId InProgress

              let maybePuzzle = State.getSession sessionId state >>= Session.lookupPuzzle puzzleIndex
              case maybePuzzle of
                Just puzzle -> do
                  -- send round puzzle
                  sendMessage connection FrontService $ RoundPuzzle sessionId puzzle
                  -- start round countdown
                  let roundCountdownValue = convert $ timeLimit $ options puzzle
                  updateState stateVar $ State.setRoundCountdown sessionId roundCountdownValue
                  mongo $ update sessionId [RoundCountdown =. roundCountdownValue]
                  case State.getRoundTimer sessionId state of
                    Just roundTimer -> do
                      _ <- repeatedStart roundTimer (roundCountdownAction roundTimer stateVar pool sessionId connection) (sDelay 1)
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
          mongo $ update sessionId [StartCountdown =. nextValue]
          sendMessage connection FrontService $ StartCountdownChanged sessionId value
        Nothing -> return ()

roundCountdownAction :: TimerIO -> MVar State -> ConnectionPool -> SessionId -> WS.Connection -> IO ()
roundCountdownAction timer stateVar pool sessionId connection = do
  let mongo = runMongoDBAction pool
  state <- readMVar stateVar
  case State.getSession sessionId state of
    Just _ -> do
      let countdownValue = State.getRoundCountdown sessionId state
      case countdownValue of
        Just 0 -> do
          stopRound stateVar pool connection sessionId
          stopTimer timer -- has to be the last statement because it kills the thread

        Just value -> do
          let nextValue = value - 1
          updateState stateVar $ State.setRoundCountdown sessionId nextValue
          mongo $ update sessionId [RoundCountdown =. nextValue]
          sendMessage connection FrontService $ RoundCountdownChanged sessionId nextValue

        Nothing -> return ()
    Nothing -> putStrLn $ "Session not found: " ++ show sessionId

app :: Config -> MVar State -> ConnectionPool -> WS.ClientApp ()
app config stateVar pool connection = do
  WS.sendTextData connection $ encode $ Envelope Messenger (ArnauxCheckin StateService)
  let mongo = runMongoDBAction pool
  forever $ do
    string <- catch
      (WS.receiveData connection)
      (\exception -> do
        print (exception :: WS.ConnectionException)
        suspend $ msDelay 500
        connectToMessenger config $ app config stateVar pool
        return "")

    case eitherDecode string :: Either String (Envelope IncomingMessage) of
      Right Envelope {message} -> case message of

        CreatePuzzle puzzle -> do
          puzzleId <- mongo $ insert puzzle
          sendMessage connection InitService $ PuzzleCreated puzzleId

        CreateSession game gameMasterId sessionAlias puzzleIds -> do
          puzzles <- mongo $ selectList [PuzzleId <-. puzzleIds] []
          let session = State.createSession game gameMasterId sessionAlias (entityVal <$> puzzles)
          sessionId <- mongo $ insert session
          mongo $ repsert sessionId session
          sessionTimers <- State.createTimers
          updateState stateVar $ State.addSession session sessionId sessionTimers
          sendMessage connection InitService $ SessionCreated sessionId

        JoinSession game sessionAlias participantId requestedRole connectionId -> do
          state <- readMVar stateVar
          let stateSession = State.getSessionByAlias sessionAlias state
          maybeSession <- case stateSession of
            Just s -> pure $ Just s
            Nothing -> do
              dbSession <- mongo $ selectFirst [Game ==. game, Alias ==. sessionAlias] []
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
              let role = Session.getParticipantRole participantId session
                  participantData = SessionJoinSucccess sessionId participantId role connectionId
              if requestedRole == role
                then do
                  updateState stateVar $ State.addParticipant sessionId participantId
                  mongo $ update sessionId [Participants =. Session.participants session]
                  sendMessage connection FrontService participantData
                  let sessionStateMessage = case role of
                                              Role.Player -> PlayerSessionState
                                              Role.GameMaster -> GameMasterSessionState
                  sendMessage connection FrontService $ sessionStateMessage sessionId participantId session
                else
                  sendMessage connection FrontService $ SessionJoinFailure connectionId
            Nothing -> putStrLn $ "Couldn't resolve session alias: " ++ show sessionAlias

        LeaveSession sessionId participantId -> do
          state <- readMVar stateVar
          case State.getSession sessionId state of
            Just _ -> do
              updateState stateVar $ State.removeParticipant sessionId participantId
              withSession stateVar sessionId $ \session ->
                mongo $ update sessionId [Participants =. Session.participants session]
              sendMessage connection FrontService $ ParticipantLeft sessionId participantId
            Nothing -> putStrLn $ "Session not found: " ++ show sessionId

        SetPuzzleIndex sessionId puzzleIndex -> do
          state <- readMVar stateVar
          case State.getSession sessionId state of
            Just session -> do
              updateState stateVar $
                State.setPuzzleIndex sessionId puzzleIndex
                . State.setRoundPhase sessionId Idle
              mongo $ update sessionId [PuzzleIndex =. puzzleIndex, RoundPhase =. Idle]
              case puzzleIndex of
                Just index ->
                  case Session.lookupPuzzle index session of
                    Just puzzle -> do
                      sendMessage connection FrontService $ RoundPhaseChanged sessionId Idle
                      sendMessage connection FrontService $ PuzzleChanged sessionId (Just index) (Just puzzle)
                    Nothing -> putStrLn $ "Puzzle not found: index " ++ show index
                Nothing -> do
                  sendMessage connection FrontService $ PuzzleChanged sessionId Nothing Nothing
                  sendMessage connection FrontService $ RoundPhaseChanged sessionId Idle
            Nothing -> putStrLn $ "Session not found: " ++ show sessionId

        StartRound sessionId -> do
          state <- readMVar stateVar
          case State.getSession sessionId state of
            Just session -> do
              updateState stateVar $ State.setRoundPhase sessionId Countdown
              mongo $ update sessionId [RoundPhase =. Countdown]
              let puzzleIndex = State.getPuzzleIndex sessionId state
              let maybePuzzle = puzzleIndex >>= flip Session.lookupPuzzle session
              case maybePuzzle of
                Just puzzle -> do
                  sendMessage connection SandboxService $ SetSandbox puzzle
                  sendMessage connection FrontService $ RoundPhaseChanged sessionId Countdown
                  let countdownValue = 2
                  updateState stateVar $ State.setStartCountdown sessionId countdownValue
                  mongo $ update sessionId [StartCountdown =. countdownValue]
                  case State.getStartTimer sessionId state of
                    Just timer -> do
                      _ <- repeatedStart timer (startCountdownAction timer stateVar pool sessionId connection) (sDelay 1)
                      sendMessage connection FrontService $ StartCountdownChanged sessionId $ countdownValue + 1
                    Nothing -> putStrLn $ "Timer error for session " ++ show sessionId
                Nothing -> putStrLn $ "Puzzle not found: index " ++ show puzzleIndex
            Nothing -> putStrLn $ "Session not found: " ++ show sessionId

        StopRound sessionId -> do
          state <- readMVar stateVar
          case State.getSession sessionId state of
            Just _ -> do
              State.stopTimers sessionId state
              stopRound stateVar pool connection sessionId
            Nothing -> putStrLn $ "Session not found: " ++ show sessionId

        ParticipantInput sessionId participantId input timestamp -> do
          state <- readMVar stateVar
          case State.getSession sessionId state of
            Just _ ->
              unless (State.hasCorrectSolution sessionId participantId state) $ do
                  -- this update is not synced with the database, participant input is considered perishable
                  updateState stateVar $ State.setParticipantInput sessionId participantId input
                  transaction <- State.createSandboxTransaction sessionId participantId input timestamp
                  -- this update is not synced with the database because of possible performance implications
                  updateState stateVar $ State.addSandboxTransaction transaction
                  sendMessage connection SandboxService $ EvaluateSolution (taskId transaction) input
            Nothing -> putStrLn $ "Session not found: " ++ show sessionId

        EvaluatedSolution taskId result correctness -> do
          state <- readMVar stateVar
          case State.getSandboxTransaction taskId state of
            Just SandboxTransaction{sessionId, participantId, input, time} -> do
              let solutionTime = State.getSolutionTime sessionId time state
              let solutionLength = Text.length input
              unless (State.hasCorrectSolution sessionId participantId state) $ do
                  updateState stateVar $ State.addSolution sessionId participantId $ Solution input solutionTime correctness
                  sendMessage connection FrontService $ SolutionEvaluated
                                                          sessionId
                                                          participantId
                                                          result
                                                          solutionTime
                                                          solutionLength
                                                          correctness
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
        connectToMessenger config $ app config stateVar pool
    Nothing -> fail ("Configuration file '" ++ env ++ "' was not found")
