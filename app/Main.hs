{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Control.Concurrent
import           Control.Monad          (forever)
import           Control.Monad.IO.Class (MonadIO)
import           Network.Socket         (withSocketsDo)
import qualified Network.WebSockets     as WS
import           Data.Aeson
import           Database.MongoDB
import           Control.Concurrent.Timer
import           Control.Concurrent.Suspend.Lifted

import BSON
import State (State)
import qualified State
import Message
import Identity
import Envelope
import RoundPhase
import Reference

updateState :: MVar State -> (State -> State) -> IO ()
updateState stateVar action = do
  state <- takeMVar stateVar
  putMVar stateVar $ action state

doMongoDBAction :: (MonadIO m) => Pipe -> Action m a -> m a
doMongoDBAction pipe = access pipe master "redemption-test"

sendMessage :: WS.Connection -> Identity -> OutgoingMessage -> IO ()
sendMessage connection to message =
  WS.sendTextData connection $ encode $ Envelope to message

startCountdownAction :: Timer IO -> MVar State -> SessionRef -> WS.Connection -> IO ()
startCountdownAction timer stateVar sessionId connection = do
  state <- readMVar stateVar
  let countdownValue = State.getStartCountdown sessionId state
  case countdownValue of
    Just 0 -> do
      let phase = Game
      updateState stateVar $ State.setRoundPhase sessionId phase
      sendMessage connection FrontService $ RoundPhaseChanged sessionId phase
      updateState stateVar $ State.setRoundCountdown sessionId 5
      roundTimer <- newTimer
      _ <- repeatedStart roundTimer (roundCountdownAction roundTimer stateVar sessionId connection) (sDelay 1)
      stopTimer timer
    Just value -> do
      let nextValue = value - 1
      updateState stateVar $ State.setStartCountdown sessionId nextValue
      sendMessage connection FrontService $ StartCountdownChanged sessionId nextValue
    Nothing -> return ()

roundCountdownAction :: Timer IO -> MVar State -> SessionRef -> WS.Connection -> IO ()
roundCountdownAction timer stateVar sessionId connection = do
  state <- readMVar stateVar
  let countdownValue = State.getRoundCountdown sessionId state
  case countdownValue of
    Just 0 -> do
      let phase = End
      updateState stateVar $ State.setRoundPhase sessionId phase
      sendMessage connection FrontService $ RoundPhaseChanged sessionId phase
      stopTimer timer
    Just value -> do
      let nextValue = value - 1
      updateState stateVar $ State.setRoundCountdown sessionId nextValue
      sendMessage connection FrontService $ RoundCountdownChanged sessionId nextValue
    Nothing -> return ()

app :: MVar State -> Pipe -> WS.ClientApp ()
app stateVar dbPipe connection = do
  WS.sendTextData connection $ encode $ Envelope Messenger (ArnauxCheckin StateService)
  let run = doMongoDBAction dbPipe
  forever $ do
    string <- WS.receiveData connection
    case eitherDecode string :: Either String (Envelope IncomingMessage) of
      Right Envelope {message} -> case message of
        CreateSession gameMaster -> do
          oid <- genObjectId
          let session = State.createSession (show oid) gameMaster
          updateState stateVar $ State.addSession session
          _ <- run $ insert "sessions" $ toBSON session
          sendMessage connection FrontService $ SessionCreated session
        JoinSession sessionId participant -> do
          updateState stateVar $ State.addParticipant sessionId participant
          _ <- run $ modify (select ["_id" =: sessionId] "sessions") [ "$push" =: [ "participants" =: toBSON participant] ]
          sendMessage connection FrontService $ ParticipantJoined sessionId participant
        LeaveSession sessionId participantId -> do
          updateState stateVar $ State.removeParticipant sessionId participantId
          _ <- run $ modify (select ["_id" =: sessionId] "sessions") [ "$pull" =: [ "participants" =: ["id" =: participantId] ] ]
          sendMessage connection FrontService $ ParticipantLeft sessionId participantId
        SetPuzzleIndex sessionId puzzleIndex -> do
          updateState stateVar $ State.setPuzzleIndex sessionId puzzleIndex
          _ <- run $ modify (select ["_id" =: sessionId] "sessions") [ "$set" =: [ "puzzleIndex" =: puzzleIndex ] ]
          sendMessage connection FrontService $ PuzzleIndexChanged sessionId puzzleIndex
        SetRoundPhase sessionId phase -> do
          updateState stateVar $ State.setRoundPhase sessionId phase
          _ <- run $ modify (select ["_id" =: sessionId] "sessions") [ "$set" =: [ "roundPhase" =: show phase ] ]
          case phase of
            Countdown -> do
              let countdownValue = 2
              updateState stateVar $ State.setStartCountdown sessionId countdownValue
              sendMessage connection FrontService $ StartCountdownChanged sessionId countdownValue
              timer <- newTimer
              _ <- repeatedStart timer (startCountdownAction timer stateVar sessionId connection) (sDelay 1)
              sendMessage connection FrontService $ RoundPhaseChanged sessionId phase
            _ -> sendMessage connection FrontService $ RoundPhaseChanged sessionId phase
      Left err -> putStrLn err

main :: IO ()
main = do
  stateVar <- newMVar State.empty
  pipe <- connect (host "127.0.0.1")
  withSocketsDo $ WS.runClient "localhost" 3000 "/" $ app stateVar pipe
