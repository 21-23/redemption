-- TODO de-mongo
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Main (main) where

import Control.Concurrent
    ( MVar, newMVar, putMVar, readMVar, takeMVar )
import           Control.Monad          (forever, unless, when)
import           Network.Socket         (withSocketsDo)
import qualified Network.WebSockets     as WS
import Data.Aeson ( eitherDecode, encode )
import Control.Concurrent.Timer
    ( repeatedStart, stopTimer, TimerIO )
import Control.Concurrent.Suspend.Lifted
    ( msDelay, sDelay, suspend )
import           Data.Time.Clock        (getCurrentTime)
import qualified Data.Map.Strict        as Map
import qualified Data.Yaml              as Yaml
import System.Environment ( lookupEnv )
import Data.Maybe ( fromMaybe )
import Control.Exception ( IOException, catch )
import Data.Convertible.Base ( convert )
import           Data.Convertible.Instances.Time()
import qualified Data.Text              as Text

import Config ( Config(Config, messengerHost, messengerPort) )
import State (State)
import qualified State
import Message
    ( IncomingMessage(..),
      OutgoingMessage(..) )
import Envelope ( Envelope(Envelope, message) )
import Session (Session(..), SessionId, isFull, getParticipantRole)
import qualified Session
import Puzzle (options)
import PuzzleOptions (timeLimit)
import Round (Round(..))
import RoundPhase (RoundPhase(..))
import SandboxTransaction (SandboxTransaction(..))
import Solution (Solution(Solution))
import qualified Role
import Game (Game)
import SandboxStatus (SandboxStatus(..))
import ServiceIdentity (ServiceIdentity(..), ServiceSelector(..), ServiceType(..))
import Data.Foldable (forM_)

updateState :: MVar State -> (State -> State) -> IO ()
updateState stateVar action = do
  state <- takeMVar stateVar
  putMVar stateVar $ action state

withSession :: MVar State -> SessionId -> (Session -> IO ()) -> IO ()
withSession stateVar sessionId action = do
  state <- readMVar stateVar
  forM_ (State.getSession sessionId state) action

sendMessage :: WS.Connection -> ServiceSelector -> OutgoingMessage -> IO ()
sendMessage connection to message =
  WS.sendTextData connection $ encode $ Envelope to message

stopRound :: MVar State -> WS.Connection -> SessionId -> IO ()
stopRound stateVar connection sessionId = do
  state <- readMVar stateVar
  case State.getSession sessionId state of
    Just session -> do
      updateState stateVar (State.resetSyncSolutions sessionId . State.clearSandboxTransactions . State.setRoundPhase sessionId End)
      -- TODO: de-mongo
      -- mongo $ update sessionId [RoundPhase =. End, Rounds =. Session.rounds session]
      case State.getSandboxStatus sessionId state of
        Just (Ready sandboxIdentity) -> do
          sendMessage connection (Service sandboxIdentity) ResetSandbox
          sendMessage connection (AnyOfType FrontService) $ RoundPhaseChanged sessionId End
          sendMessage connection (AnyOfType FrontService) $ Score sessionId $ Session.getPlayerRoundData session
        _ -> putStrLn $ "Sandbox state error: " ++ show sessionId
    Nothing -> putStrLn $ "Session not found: " ++ show sessionId

startCountdownAction :: TimerIO -> MVar State -> SessionId -> WS.Connection -> IO ()
startCountdownAction timer stateVar sessionId connection = do
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
              -- TODO: de-mongo
              -- withSession stateVar sessionId $ \session ->
              --   mongo $ update sessionId [Rounds =. Session.rounds session]
              -- change phase to 'in progress'
              updateState stateVar $ State.setRoundPhase sessionId InProgress
              -- TODO: de-mongo
              -- mongo $ update sessionId [RoundPhase =. InProgress]
              sendMessage connection (AnyOfType FrontService) $ RoundPhaseChanged sessionId InProgress

              let maybePuzzle = State.getSession sessionId state >>= Session.lookupPuzzle puzzleIndex
              case maybePuzzle of
                Just puzzle -> do
                  -- send round puzzle
                  sendMessage connection (AnyOfType FrontService) $ RoundPuzzle sessionId puzzle
                  -- start round countdown
                  let roundCountdownValue = convert $ timeLimit $ options puzzle
                  updateState stateVar $ State.setRoundCountdown sessionId roundCountdownValue
                  -- TODO: de-mongo
                  -- mongo $ update sessionId [RoundCountdown =. roundCountdownValue]
                  case State.getRoundTimer sessionId state of
                    Just roundTimer -> do
                      _ <- repeatedStart roundTimer (roundCountdownAction roundTimer stateVar sessionId connection) (sDelay 1)
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
          -- TODO: de-mongo
          -- mongo $ update sessionId [StartCountdown =. nextValue]
          sendMessage connection (AnyOfType FrontService) $ StartCountdownChanged sessionId value
        Nothing -> return ()

roundCountdownAction :: TimerIO -> MVar State -> SessionId -> WS.Connection -> IO ()
roundCountdownAction timer stateVar sessionId connection = do
  state <- readMVar stateVar
  case State.getSession sessionId state of
    Just _ -> do
      let countdownValue = State.getRoundCountdown sessionId state
      case countdownValue of
        Just 0 -> do
          stopRound stateVar connection sessionId
          case State.getSolutionSyncTimer sessionId state of
            Just solutionSyncTimer -> stopTimer solutionSyncTimer
            Nothing -> putStrLn $ "SolutionSyncTimer not found: " ++ show sessionId
          stopTimer timer -- has to be the last statement because it kills the thread

        Just value -> do
          let nextValue = value - 1
          updateState stateVar $ State.setRoundCountdown sessionId nextValue
          -- TODO: de-mongo
          -- mongo $ update sessionId [RoundCountdown =. nextValue]
          sendMessage connection (AnyOfType FrontService) $ RoundCountdownChanged sessionId nextValue

        Nothing -> return ()
    Nothing -> putStrLn $ "Session not found: " ++ show sessionId

solutionSyncAction :: MVar State -> SessionId -> WS.Connection -> IO ()
solutionSyncAction stateVar sessionId connection = do
  state <- readMVar stateVar
  case State.getSession sessionId state of
    Just session ->
      unless (State.syncSolutionsEmpty sessionId state) $ do
        sendMessage connection (AnyOfType FrontService) $ SolutionSync sessionId session
        updateState stateVar $ State.resetSyncSolutions sessionId
    Nothing      -> putStrLn $ "Session not found: " ++ show sessionId

requestSandbox :: WS.Connection -> MVar State -> SessionId -> Game -> IO ()
requestSandbox connection stateVar sessionId game = do
  updateState stateVar $ State.setSandboxStatus sessionId Requested
  state <- readMVar stateVar
  case State.identity state of
    Just identity ->
      sendMessage connection (AnyOfType ContainerService) $ ServiceRequest identity (ServiceIdentity.SandboxService game)
    Nothing -> return ()

app :: Config -> MVar State -> WS.ClientApp ()
app config stateVar connection = do
  WS.sendTextData connection $ encode $ Envelope Messenger (ArnauxCheckin StateService)
  forever $ do
    string <- catch
      (WS.receiveData connection)
      (\exception -> do
        print (exception :: WS.ConnectionException)
        suspend $ msDelay 500
        connectToMessenger config $ app config stateVar
        return "")

    case eitherDecode string :: Either String (Envelope IncomingMessage) of
      Right Envelope {message} -> case message of

        CheckedIn identity ->
          updateState stateVar $ \state -> state { State.identity = Just identity }

        CreatePuzzle puzzle -> do
          -- TODO: de-mongo
          -- puzzleId <- mongo $ insert puzzle
          let puzzleId = undefined
          sendMessage connection (AnyOfType InitService) $ PuzzleCreated puzzleId

        CreateSession game gameMasterId sessionAlias puzzleIds participantLimit -> do
          -- TODO de-mongo
          -- puzzles <- mongo $ selectList [PuzzleId <-. puzzleIds] []
          let puzzles = undefined
          let session = State.createSession game gameMasterId sessionAlias puzzles participantLimit
          -- TODO: de-mongo
          let sessionId = undefined
          -- sessionId <- mongo $ insert session
          -- TODO: de-mongo
          -- mongo $ repsert sessionId session
          sendMessage connection (AnyOfType InitService) $ SessionCreated sessionId

        JoinSession game sessionAlias participantId requestedRole connectionId -> do
          state <- readMVar stateVar
          let stateSession = State.getSessionByAlias game sessionAlias state
          maybeSession <- case stateSession of
            Just s -> pure $ Just s
            Nothing -> do
              -- TODO: de-mongo
              -- dbSession <- mongo $ selectFirst [Game ==. game, Alias ==. sessionAlias] []
              let dbSession = undefined
              case dbSession of
                Just entity -> do
                  -- TODO: de-mongo
                  -- let sessionId = entityKey entity
                  let sessionId = undefined
                      -- TODO: de-mongo
                      -- session = entityVal entity
                      session = undefined
                  sessionTimers <- State.createTimers
                  updateState stateVar $ State.addSession session sessionId sessionTimers
                  requestSandbox connection stateVar sessionId game
                  return $ Just (sessionId, session)
                Nothing -> return Nothing

          case maybeSession of
            Just (sessionId, session) -> do
              let role = getParticipantRole participantId session
                  participantData = SessionJoinSucccess sessionId participantId role connectionId
              if requestedRole == role
                then do
                  if not (isFull session) || role == Role.GameMaster
                    then do
                      updateState stateVar $ State.addParticipant sessionId participantId
                      -- TODO: de-mongo
                      -- mongo $ update sessionId [Participants =. Session.participants session]
                      sendMessage connection (AnyOfType FrontService) participantData
                      let sessionStateMessage = case role of
                                                  Role.Player -> PlayerSessionState
                                                  Role.GameMaster -> GameMasterSessionState
                      sendMessage connection (AnyOfType FrontService) $ sessionStateMessage sessionId participantId session
                    else
                      sendMessage connection (AnyOfType FrontService) $ SessionJoinFailure connectionId "session participant limit exceeded"
                else
                  sendMessage connection (AnyOfType FrontService) $ SessionJoinFailure connectionId "role mismatch"
            Nothing -> putStrLn $ "Couldn't resolve session alias: " ++ show sessionAlias

        LeaveSession sessionId participantId -> do
          state <- readMVar stateVar
          case State.getSession sessionId state of
            Just _ -> do
              updateState stateVar $ State.removeParticipant sessionId participantId
              -- TODO: de-mongo
              -- withSession stateVar sessionId $ \session ->
              --   mongo $ update sessionId [Participants =. Session.participants session]
              sendMessage connection (AnyOfType FrontService) $ ParticipantLeft sessionId participantId
            Nothing -> putStrLn $ "Session not found: " ++ show sessionId

        SetPuzzleIndex sessionId puzzleIndex -> do
          state <- readMVar stateVar
          case State.getSession sessionId state of
            Just session -> do
              updateState stateVar $
                State.setPuzzleIndex sessionId puzzleIndex
                . State.setRoundPhase sessionId Idle
              -- TODO: de-mongo
              -- mongo $ update sessionId [PuzzleIndex =. puzzleIndex, RoundPhase =. Idle]
              case puzzleIndex of
                Just index ->
                  case Session.lookupPuzzle index session of
                    Just puzzle -> do
                      sendMessage connection (AnyOfType FrontService) $ RoundPhaseChanged sessionId Idle
                      sendMessage connection (AnyOfType FrontService) $ PuzzleChanged sessionId (Just index) (Just puzzle)
                    Nothing -> putStrLn $ "Puzzle not found: index " ++ show index
                Nothing -> do
                  sendMessage connection (AnyOfType FrontService) $ PuzzleChanged sessionId Nothing Nothing
                  sendMessage connection (AnyOfType FrontService) $ RoundPhaseChanged sessionId Idle
            Nothing -> putStrLn $ "Session not found: " ++ show sessionId

        StartRound sessionId -> do
          state <- readMVar stateVar
          case State.getSession sessionId state of
            Just session ->
              case Session.sandboxStatus session of
                Ready sandboxIdentity -> do
                  updateState stateVar $ State.setRoundPhase sessionId Countdown
                  -- TODO: de-mongo
                  -- mongo $ update sessionId [RoundPhase =. Countdown]
                  let puzzleIndex = State.getPuzzleIndex sessionId state
                  let maybePuzzle = puzzleIndex >>= flip Session.lookupPuzzle session
                  case maybePuzzle of
                    Just puzzle -> do
                      sendMessage connection (Service sandboxIdentity) $ SetSandbox puzzle
                      sendMessage connection (AnyOfType FrontService) $ RoundPhaseChanged sessionId Countdown
                      let countdownValue = 2
                      updateState stateVar $ State.setStartCountdown sessionId countdownValue
                      -- TODO: de-mongo
                      -- mongo $ update sessionId [StartCountdown =. countdownValue]
                      let timers = (,)
                                    <$> State.getStartTimer sessionId state
                                    <*> State.getSolutionSyncTimer sessionId state
                      case timers of
                        Just (startTimer, solutionSyncTimer) -> do
                          _ <- repeatedStart startTimer (startCountdownAction startTimer stateVar sessionId connection) (sDelay 1)
                          _ <- repeatedStart solutionSyncTimer (solutionSyncAction stateVar sessionId connection) (msDelay 200)
                          sendMessage connection (AnyOfType FrontService) $ StartCountdownChanged sessionId $ countdownValue + 1
                        Nothing -> putStrLn $ "Timer error for session " ++ show sessionId
                    Nothing -> putStrLn $ "Puzzle not found: index " ++ show puzzleIndex
                Requested ->
                  putStrLn $ "Session can't be started when sandbox is not ready: " ++ show sessionId
            Nothing -> putStrLn $ "Session not found: " ++ show sessionId

        StopRound sessionId -> do
          state <- readMVar stateVar
          case State.getSession sessionId state of
            Just _ -> do
              State.stopTimers sessionId state
              stopRound stateVar connection sessionId
            Nothing -> putStrLn $ "Session not found: " ++ show sessionId

        ParticipantInput sessionId participantId input timestamp -> do
          state <- readMVar stateVar
          case State.getSession sessionId state of
            Just Session{roundPhase, sandboxStatus} ->
              when (roundPhase == InProgress) $
                unless (State.hasCorrectSolution sessionId participantId state) $
                  case sandboxStatus of
                    Ready sandboxIdentity -> do
                      -- this update is not synced with the database, participant input is considered perishable
                      updateState stateVar $ State.setParticipantInput sessionId participantId input
                      transaction <- State.createSandboxTransaction sessionId participantId input timestamp
                      -- this update is not synced with the database because of possible performance implications
                      updateState stateVar $ State.updateSandboxTransaction transaction

                      sendMessage connection (Service sandboxIdentity) $ EvaluateSolution (taskId transaction) input
                    Requested ->
                      putStrLn $ "Can't evaluate solutions when sandbox is not ready: " ++ show sessionId
            Nothing -> putStrLn $ "Session not found: " ++ show sessionId

        EvaluatedSolution taskId result correctness -> do
          state <- readMVar stateVar
          case State.getSandboxTransaction taskId state of
            Just SandboxTransaction{sessionId, participantId, input, time} -> do
              let solutionTime = State.getSolutionTime sessionId time state
              let solutionLength = Text.length input
              unless (State.hasCorrectSolution sessionId participantId state) $ do
                  updateState stateVar $ State.addSolution sessionId participantId $ Solution input solutionTime correctness
                  sendMessage connection (AnyOfType FrontService) $ SolutionEvaluated
                                                          sessionId
                                                          participantId
                                                          result
                                                          solutionTime
                                                          solutionLength
                                                          correctness
            Nothing -> putStrLn $ "Transaction not found: " ++ show taskId

        ServiceRequestFulfilled serviceIdentity@(ServiceIdentity serviceType _) -> do
          state <- readMVar stateVar
          case State.getSessionForSandbox state serviceType of
            Just sessionId -> do
              updateState stateVar $ State.setSandboxStatus sessionId (Ready serviceIdentity)
              sendMessage connection (AnyOfType FrontService) $ SessionSandboxReady sessionId
            Nothing -> putStrLn $ "No session found for created sandbox of type: " ++ show serviceType

      Left err -> putStrLn err

connectToMessenger :: Config -> WS.ClientApp () -> IO ()
connectToMessenger config@Config{messengerHost, messengerPort} clientApp =
  catch
    (withSocketsDo $ WS.runClient messengerHost messengerPort "/" clientApp)
    (\exception -> do
      print (exception :: IOException)
      suspend $ msDelay 500
      connectToMessenger config clientApp)

main :: IO ()
main = do
  maybeEnv <- lookupEnv "redemption_environment"
  let env = fromMaybe "dev" maybeEnv
  -- TODO: de-mongo
  -- maybeConfig <- Yaml.decodeFileEither ("conf/" ++ env ++ ".yaml")
  maybeConfig <- Yaml.decodeFileEither ("conf/" ++ env ++ ".yaml") :: IO (Either Yaml.ParseException Config)
  case maybeConfig of
    Right config -> do
      stateVar <- newMVar State.empty
      mempty
      -- TODO de-mongo
      -- withMongoDBPool "qd" (Config.mongoDBHost config) (PortNumber 27017) Nothing 2 1 20000 $ \pool ->
      --   connectToMessenger config $ app config stateVar pool
    Left _ -> fail ("Configuration file '" ++ env ++ "' was not found")
