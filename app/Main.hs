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


import Config
import State (State)
import qualified State
import Message
import Identity
import Envelope
import Session (EntityField(..), SessionId)
import qualified Session as Session
import Puzzle
import Round (Round(..))
import RoundPhase
-- import Solution


updateState :: MVar State -> (State -> State) -> IO ()
updateState stateVar action = do
  state <- takeMVar stateVar
  putMVar stateVar $ action state

sendMessage :: WS.Connection -> Identity -> OutgoingMessage -> IO ()
sendMessage connection to message =
  WS.sendTextData connection $ encode $ Envelope to message

startCountdownAction :: Timer IO -> MVar State -> ConnectionPool -> SessionId -> WS.Connection -> IO ()
startCountdownAction timer stateVar pool sessionId connection = do
  let mongo = runMongoDBAction pool
  state <- readMVar stateVar
  let countdownValue = State.getStartCountdown sessionId state
  case countdownValue of
    Just 0 ->
      case State.getPuzzleIndex sessionId state of
        Just puzzleIndex -> do
          case State.getSession sessionId state of
            Just session -> do
              -- create a new round
              currentTime <- getCurrentTime
              updateState stateVar $ State.addRound sessionId $ Round puzzleIndex currentTime Map.empty
              mongo $ update sessionId [Rounds =. Session.rounds session]
              -- change phase to 'game'
              updateState stateVar $ State.setRoundPhase sessionId InProgress
              mongo $ update sessionId [RoundPhase =. InProgress]
              sendMessage connection FrontService $ RoundPhaseChanged sessionId InProgress

              let maybePuzzle = State.getSession sessionId state >>= Session.lookupPuzzle puzzleIndex
              case maybePuzzle of
                Just puzzle -> do
                  -- send round puzzle
                  sendMessage connection FrontService $ RoundPuzzle sessionId puzzle
                  -- start round countdown
                  let roundCountdownValue = convert $ timeLimit puzzle
                  updateState stateVar $ State.setRoundCountdown sessionId roundCountdownValue
                  mongo $ update sessionId [RoundCountdown =. roundCountdownValue]
                  roundTimer <- newTimer
                  _ <- repeatedStart roundTimer (roundCountdownAction roundTimer stateVar pool sessionId connection) (sDelay 1)
                  stopTimer timer -- has to be the last statement because it kills the thread
                Nothing -> do
                  putStrLn $ "Puzzle not found: index " ++ show puzzleIndex
                  stopTimer timer -- has to be the last statement because it kills the thread

            Nothing -> do
              putStrLn $ "Session not found: " ++ show sessionId
              stopTimer timer -- has to be the last statement because it kills the thread

        Nothing -> stopTimer timer -- has to be the last statement because it kills the thread
    Just value -> do
      let nextValue = value - 1
      updateState stateVar $ State.setStartCountdown sessionId nextValue
      sendMessage connection FrontService $ StartCountdownChanged sessionId nextValue
    Nothing -> return ()

roundCountdownAction :: Timer IO -> MVar State -> ConnectionPool -> SessionId -> WS.Connection -> IO ()
roundCountdownAction timer stateVar pool sessionId connection = do
  let mongo = runMongoDBAction pool
  state <- readMVar stateVar
  let countdownValue = State.getRoundCountdown sessionId state
  case countdownValue of
    Just 0 -> do
      updateState stateVar $ State.setRoundPhase sessionId End
      mongo $ update sessionId [RoundPhase =. End]
      sendMessage connection FrontService $ RoundPhaseChanged sessionId End
      stopTimer timer -- has to be the last statement because it kills the thread
    Just value -> do
      let nextValue = value - 1
      updateState stateVar $ State.setRoundCountdown sessionId nextValue
      mongo $ update sessionId [RoundCountdown =. nextValue]
      sendMessage connection FrontService $ RoundCountdownChanged sessionId nextValue
    Nothing -> return ()

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

        CreateSession gameMasterId puzzleIds -> do
          puzzles <- mongo $ selectList [PuzzleId <-. puzzleIds] []
          let session = State.createSession gameMasterId (entityVal <$> puzzles)
          sessionId <- mongo $ insert session
          mongo $ repsert sessionId session
          updateState stateVar $ State.addSession session sessionId
          sendMessage connection InitService $ SessionCreated sessionId

        JoinSession sessionId participantId -> do
          state <- readMVar stateVar
          let stateSession = State.getSession sessionId state
          maybeSession <- case stateSession of
            Just s -> pure $ Just s
            Nothing -> do
              dbSession <- mongo $ get sessionId
              case dbSession of
                Just session -> do
                  updateState stateVar $ State.addSession session sessionId
                  return $ Just session
                Nothing -> return Nothing

          case maybeSession of
            Just session -> do
              updateState stateVar $ State.addParticipant sessionId participantId
              mongo $ update sessionId [Participants =. Session.participants session]
              let role = Session.getParticipantRole participantId session
                  participantData = ParticipantJoined sessionId participantId role
              sendMessage connection FrontService participantData
              sendMessage connection FrontService $ PlayerSessionState sessionId participantId session
            Nothing -> putStrLn $ "Session not found: " ++ show sessionId

        LeaveSession sessionId participantId -> do
          updateState stateVar $ State.removeParticipant sessionId participantId
          state <- readMVar stateVar
          case State.getSession sessionId state of
            Just session -> do
              mongo $ update sessionId [Participants =. Session.participants session]
              sendMessage connection FrontService $ ParticipantLeft sessionId participantId
            Nothing -> putStrLn $ "Session not found: " ++ show sessionId

        SetPuzzleIndex sessionId puzzleIndex -> do
          updateState stateVar $ State.setPuzzleIndex sessionId puzzleIndex
          state <- readMVar stateVar
          case State.getSession sessionId state of
            Just session -> do
              mongo $ update sessionId [PuzzleIndex =. Session.puzzleIndex session]
              case Session.lookupPuzzle puzzleIndex session of
                Just puzzle -> do
                  sendMessage connection FrontService $ PuzzleChanged sessionId puzzleIndex puzzle
                Nothing -> putStrLn $ "Puzzle not found: index " ++ show puzzleIndex
            Nothing -> putStrLn $ "Session not found: " ++ show sessionId

        StartRound sessionId -> do
          state <- readMVar stateVar
          case State.getSession sessionId state of
            Just session -> do
              updateState stateVar $ State.setRoundPhase sessionId Countdown
              mongo $ update sessionId [RoundPhase =. Countdown]
              let puzzleIndex = State.getPuzzleIndex sessionId state
              let maybePuzzle = puzzleIndex >>= (flip Session.lookupPuzzle) session
              case maybePuzzle of
                Just puzzle -> do
                  sendMessage connection SandboxService $ SetSandbox puzzle
                  sendMessage connection FrontService $ RoundPhaseChanged sessionId Countdown
                  let countdownValue = 2
                  updateState stateVar $ State.setStartCountdown sessionId countdownValue
                  mongo $ update sessionId [StartCountdown =. countdownValue]
                  timer <- newTimer
                  _ <- repeatedStart timer (startCountdownAction timer stateVar pool sessionId connection) (sDelay 1)
                  sendMessage connection FrontService $ StartCountdownChanged sessionId countdownValue
                Nothing -> putStrLn $ "Puzzle not found: index " ++ show puzzleIndex
            Nothing -> putStrLn $ "Session not found: " ++ show sessionId

        _ -> return ()

      Left err -> putStrLn err

--         ParticipantInput sessionId participantId input -> do
--           updateState stateVar $ State.setParticipantInput sessionId participantId input
--           sendMessage connection FrontService $ ParticipantInputChanged sessionId participantId $ length input
--           sendMessage connection SandboxService $ EvaluateSolution sessionId participantId input
--         EvaluatedSolution sessionId participantId solution True -> do
--           currentTime <- getCurrentTime
--           updateState stateVar $ State.addSolution sessionId participantId $ Solution solution currentTime
--           sendMessage connection FrontService $ SolutionEvaluated sessionId participantId solution True
--         EvaluatedSolution sessionId participantId solution False ->
--           sendMessage connection FrontService $ SolutionEvaluated sessionId participantId solution False
--       Left err -> putStrLn err

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
