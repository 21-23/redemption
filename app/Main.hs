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
-- import           Control.Concurrent.Timer
import           Control.Concurrent.Suspend.Lifted
-- import           Data.Time.Clock        (getCurrentTime)
-- import qualified Data.Map               as Map
import qualified Data.Yaml              as Yaml
import           System.Environment
import           Data.Maybe
import           Control.Exception


import Config
import State (State)
import qualified State
import Message
import Identity
import Envelope
import Session
import Puzzle
-- import Round (Round(..))
-- import RoundPhase
-- import Solution

updateState :: MVar State -> (State -> State) -> IO ()
updateState stateVar action = do
  state <- takeMVar stateVar
  putMVar stateVar $ action state

-- doMongoDBAction :: (MonadIO m) => Pipe -> Action m a -> m a
-- doMongoDBAction pipe = access pipe master "_qd"

sendMessage :: WS.Connection -> Identity -> OutgoingMessage -> IO ()
sendMessage connection to message =
  WS.sendTextData connection $ encode $ Envelope to message

-- startCountdownAction :: Timer IO -> MVar State -> SessionRef -> WS.Connection -> IO ()
-- startCountdownAction timer stateVar sessionId connection = do
--   state <- readMVar stateVar
--   let countdownValue = State.getStartCountdown sessionId state
--   case countdownValue of
--     Just 0 ->
--       case State.getPuzzleIndex sessionId state of
--         Just puzzleIndex -> do
--           -- create a new round
--           currentTime <- getCurrentTime
--           updateState stateVar $ State.addRound sessionId $ Round puzzleIndex currentTime Map.empty
--           -- change phase to 'game'
--           updateState stateVar $ State.setRoundPhase sessionId Game
--           sendMessage connection FrontService $ RoundPhaseChanged sessionId Game
--           -- start round countdown
--           updateState stateVar $ State.setRoundCountdown sessionId 5
--           roundTimer <- newTimer
--           _ <- repeatedStart roundTimer (roundCountdownAction roundTimer stateVar sessionId connection) (sDelay 1)
--           stopTimer timer
--         Nothing -> return ()
--     Just value -> do
--       let nextValue = value - 1
--       updateState stateVar $ State.setStartCountdown sessionId nextValue
--       sendMessage connection FrontService $ StartCountdownChanged sessionId nextValue
--     Nothing -> return ()
--
-- roundCountdownAction :: Timer IO -> MVar State -> SessionRef -> WS.Connection -> IO ()
-- roundCountdownAction timer stateVar sessionId connection = do
--   state <- readMVar stateVar
--   let countdownValue = State.getRoundCountdown sessionId state
--   case countdownValue of
--     Just 0 -> do
--       let phase = End
--       updateState stateVar $ State.setRoundPhase sessionId phase
--       sendMessage connection FrontService $ RoundPhaseChanged sessionId phase
--       stopTimer timer
--     Just value -> do
--       let nextValue = value - 1
--       updateState stateVar $ State.setRoundCountdown sessionId nextValue
--       sendMessage connection FrontService $ RoundCountdownChanged sessionId nextValue
--     Nothing -> return ()

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
          updateState stateVar $ State.addSession session sessionId
          sendMessage connection InitService $ SessionCreated sessionId

        JoinSession sessionId participantId -> do
          updateState stateVar $ State.addParticipant sessionId participantId
          state <- readMVar stateVar
          case State.getSession sessionId state of
            Just session -> do
              mongo $ update sessionId [Participants =. participants session]
              let role = Session.getParticipantRole participantId session
                  participantData = ParticipantJoined sessionId participantId role
              sendMessage connection FrontService participantData
              sendMessage connection FrontService $ PlayerSessionState sessionId participantId session
            Nothing -> putStrLn $ "Session not found: " ++ show sessionId

        _ -> return ()

      Left err -> putStrLn err

-- app :: MVar State -> Pipe -> WS.ClientApp ()
-- app stateVar dbPipe connection = do
--   WS.sendTextData connection $ encode $ Envelope Messenger (ArnauxCheckin StateService)
--   let run = doMongoDBAction dbPipe
--   forever $ do
--     string <- WS.receiveData connection
--     case eitherDecode string :: Either String (Envelope IncomingMessage) of
--       Right Envelope {message} -> case message of
--         -- PuzzleRef -> IO (Maybe Document)
--         -- mapM ::  (b -> a c) -> [b] -> a [c]
--         -- c = Maybe Document
--         CreateSession gameMaster puzzleIds -> do
--           puzzles <- catMaybes <$> mapM (\pId -> run $ findOne $ select ["_id" =: pId] "puzzles") puzzleIds
--           oid <- genObjectId
--           let session = State.createSession (show oid) gameMaster puzzles
--           updateState stateVar $ State.addSession session
--           _ <- run $ insert "sessions" $ toBSON session
--           sendMessage connection FrontService $ SessionCreated session
--         JoinSession sessionId participant -> do
--           updateState stateVar $ State.addParticipant sessionId participant
--           _ <- run $ modify (select ["_id" =: sessionId] "sessions") [ "$push" =: [ "participants" =: toBSON participant] ]
--           sendMessage connection FrontService $ ParticipantJoined sessionId participant
--         LeaveSession sessionId participantId -> do
--           updateState stateVar $ State.removeParticipant sessionId participantId
--           _ <- run $ modify (select ["_id" =: sessionId] "sessions") [ "$pull" =: [ "participants" =: ["id" =: participantId] ] ]
--           sendMessage connection FrontService $ ParticipantLeft sessionId participantId
--         SetPuzzleIndex sessionId puzzleIndex -> do
--           updateState stateVar $ State.setPuzzleIndex sessionId puzzleIndex
--           _ <- run $ modify (select ["_id" =: sessionId] "sessions") [ "$set" =: [ "puzzleIndex" =: puzzleIndex ] ]
--           sendMessage connection FrontService $ PuzzleIndexChanged sessionId puzzleIndex
--         SetRoundPhase sessionId phase -> do
--           updateState stateVar $ State.setRoundPhase sessionId phase
--           _ <- run $ modify (select ["_id" =: sessionId] "sessions") [ "$set" =: [ "roundPhase" =: show phase ] ]
--           sendMessage connection FrontService $ RoundPhaseChanged sessionId phase
--           case phase of
--             Countdown -> do
--               let countdownValue = 2
--               updateState stateVar $ State.setStartCountdown sessionId countdownValue
--               timer <- newTimer
--               _ <- repeatedStart timer (startCountdownAction timer stateVar sessionId connection) (sDelay 1)
--               sendMessage connection FrontService $ StartCountdownChanged sessionId countdownValue
--             End -> do
--               state <- readMVar stateVar
--               let score = State.getLastRoundScore sessionId state
--               sendMessage connection FrontService $ RoundScore sessionId score
--
--             _ -> sendMessage connection FrontService $ RoundPhaseChanged sessionId phase
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
--         CreatePuzzle puzzleData@Puzzle{puzzleId} -> do
--           puzzle <- case puzzleId of
--                       Just _ -> return puzzleData
--                       Nothing -> do
--                         oid <- genObjectId
--                         return puzzleData { puzzleId = Just $ show oid }
--           _ <- run $ insert "puzzles" $ toBSON puzzle
--           sendMessage connection InitService $ PuzzleCreated puzzle
--
--
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
      withMongoDBConn "_qd" (Config.mongoDBHost config) (PortNumber 27017) Nothing 2000 $ \pool ->
        connectToMessenger config $ app stateVar pool
    Nothing -> fail ("Configuration file '" ++ env ++ "' was not found")
