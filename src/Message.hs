{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Message where

import Control.Monad
import Data.Text (Text)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Data.Aeson
import Data.Semigroup
import Data.Time.Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Maybe (fromMaybe)
import Data.UUID (UUID)
import Data.ByteString.Lazy (ByteString)

import Identity
import Participant
import Session
import RoundPhase
import Puzzle (Puzzle, PuzzleId, name, input, expected, hidden, options, toSimpleJSON)
import PuzzleOptions (timeLimit, sandboxSettings)
import Role
import UUIDPersistField()
import PlayerRoundData (PlayerRoundData)
import SolutionCorrectness (SolutionCorrectness(..))
import Game (Game)

type ConnectionId = String

data IncomingMessage
  = CreateSession Game ParticipantUid SessionAlias [PuzzleId]
  | JoinSession Game SessionAlias ParticipantUid Role ConnectionId
  | LeaveSession SessionId ParticipantUid
  | SetPuzzleIndex SessionId (Maybe Int)
  | StartRound SessionId
  | StopRound SessionId
  | ParticipantInput SessionId ParticipantUid Text UTCTime
  | EvaluatedSolution UUID (Either Text ByteString) SolutionCorrectness
  | CreatePuzzle Puzzle

data OutgoingMessage
  = ArnauxCheckin Identity
  | SessionCreated SessionId
  | SessionJoinSucccess SessionId ParticipantUid Role ConnectionId
  | SessionJoinFailure ConnectionId
  | ParticipantLeft SessionId ParticipantUid
  | KickParticipant SessionId ParticipantUid
  | PuzzleChanged SessionId (Maybe Int) (Maybe Puzzle)
  | RoundPhaseChanged SessionId RoundPhase
  | SetSandbox Puzzle
  | ResetSandbox
  | StartCountdownChanged SessionId Int
  | RoundCountdownChanged SessionId Int
  | RoundPuzzle SessionId Puzzle
  | EvaluateSolution UUID Text
  | SolutionEvaluated SessionId ParticipantUid (Either Text ByteString) NominalDiffTime Int SolutionCorrectness
  | Score SessionId [PlayerRoundData]
  | PuzzleCreated PuzzleId
  | PlayerSessionState SessionId ParticipantUid Session
  | GameMasterSessionState SessionId ParticipantUid Session

toName :: OutgoingMessage -> Text
toName ArnauxCheckin {}           = "checkin"
toName SessionCreated {}          = "session.created"
toName SessionJoinSucccess {}     = "sessionJoin.result"
toName SessionJoinFailure {}      = "sessionJoin.result"
toName ParticipantLeft {}         = "participant.left"
toName KickParticipant {}         = "participant.kick"
toName PuzzleChanged {}           = "puzzle.changed"
toName RoundPhaseChanged {}       = "roundPhase.changed"
toName SetSandbox {}              = "sandbox.set"
toName ResetSandbox {}            = "sandbox.reset"
toName StartCountdownChanged {}   = "startCountdown.changed"
toName RoundCountdownChanged {}   = "roundCountdown.changed"
toName RoundPuzzle {}             = "puzzle"
toName EvaluateSolution {}        = "solution.evaluate"
toName SolutionEvaluated {}       = "solution.evaluated"
toName Score {}                   = "score"
toName PuzzleCreated {}           = "puzzle.created"
toName PlayerSessionState {}      = "player.sessionState"
toName GameMasterSessionState {}  = "gameMaster.sessionState"

instance ToJSON OutgoingMessage where
  toJSON message = object $ ["name" .= toName message] <> toValue message
    where
      toValue (ArnauxCheckin identity) = ["identity" .= identity]
      toValue (SessionCreated sessionId) = ["sessionId" .= sessionId]
      toValue (SessionJoinSucccess sessionId participantId role connectionId) =
        [ "sessionId" .= sessionId
        , "participantId" .= participantId
        , "role" .= role
        , "connectionId" .= connectionId
        ]
      toValue (SessionJoinFailure connectionId) =
        [ "connectionId" .= connectionId
        ]
      toValue (ParticipantLeft sessionId participantId) =
        [ "sessionId" .= sessionId
        , "participantId" .= participantId
        ]
      toValue (KickParticipant sessionId participantId) =
        [ "sessionId" .= sessionId
        , "participantId" .= participantId
        ]
      toValue (PuzzleChanged sessionId puzzleIndex puzzle) =
        [ "sessionId" .= sessionId
        , "puzzleIndex" .= puzzleIndex
        , "puzzleName" .= (name <$> puzzle)
        , "timeLimit" .= (timeLimit . options <$> puzzle)
        ]
      toValue (RoundPhaseChanged sessionId phase) =
        [ "sessionId" .= sessionId
        , "roundPhase" .= phase
        ]
      toValue (StartCountdownChanged sessionId value) =
        [ "sessionId" .= sessionId
        , "startCountdown" .= value
        ]
      toValue (RoundCountdownChanged sessionId value) =
        [ "sessionId" .= sessionId
        , "roundCountdown" .= value
        ]
      toValue (SetSandbox puzzle) =
        [ "input" .= input puzzle
        , "expected" .= expected puzzle
        , "hidden" .= hidden puzzle
        , "settings" .= sandboxSettings (options puzzle)
        ]
      toValue ResetSandbox = []
      toValue (RoundPuzzle sessionId puzzle) =
        [ "sessionId" .= sessionId
        , "input" .= input puzzle
        , "expected" .= expected puzzle
        , "timeLimit" .= timeLimit (options puzzle)
        ]
      toValue (EvaluateSolution taskId solution) =
        [ "taskId" .= taskId
        , "solution" .= solution
        ]
      toValue (SolutionEvaluated sessionId participantId result time len correct) =
        [ "sessionId" .= sessionId
        , "participantId" .= participantId
        , "error" .= either Just (const Nothing) result
        , "result" .= either (const Nothing) (Just . decodeUtf8) result
        , "length" .= len
        , "time" .= time
        , "correct" .= correct
        ]
      toValue (Score sessionId playerRoundData) =
        [ "sessionId" .= sessionId
        , "players" .= playerRoundData
        ]
      toValue (PuzzleCreated puzzleId) = [ "puzzleId" .= puzzleId ]
      toValue (PlayerSessionState sessionId participantId session) =
        [ "sessionId" .= sessionId
        , "participantId" .= participantId
        , "puzzleIndex" .= puzzleIndex session
        , "puzzleCount" .= length (puzzles session)
        , "puzzle" .= (toSimpleJSON <$> getPuzzleForSessionState session)
        , "roundPhase" .= roundPhase session
        , "roundCountdown" .= roundCountdown session
        , "startCountdown" .= startCountdown session
        , "solution" .= getSolution participantId session
        , "solved" .= hasCorrectSolution participantId session
        ]
      toValue (GameMasterSessionState sessionId participantId session) =
        [ "sessionId" .= sessionId
        , "participantId" .= participantId
        , "puzzleIndex" .= puzzleIndex session
        , "puzzleCount" .= length (puzzles session)
        , "puzzle" .= do
            index <- puzzleIndex session
            puzzle <- lookupPuzzle index session
            return $ toSimpleJSON puzzle
        , "roundPhase" .= roundPhase session
        , "roundCountdown" .= roundCountdown session
        , "startCountdown" .= startCountdown session
        , "players" .= getPlayerRoundData session
        ]

getPuzzleForSessionState :: Session -> Maybe Puzzle
getPuzzleForSessionState session =
  let getPuzzle = do
                    index <- puzzleIndex session
                    lookupPuzzle index session
   in case roundPhase session of
        InProgress -> getPuzzle
        End        -> getPuzzle
        _          -> Nothing

instance FromJSON IncomingMessage where
  parseJSON (Object message) = do
    name <- message .: "name"
    case name of
      String "session.create"          -> CreateSession
        <$> message .: "game"
        <*> message .: "gameMasterId"
        <*> message .: "alias"
        <*> message .: "puzzles"
      String "session.join"            -> JoinSession
        <$> message .: "game"
        <*> message .: "sessionId"
        <*> message .: "participantId"
        <*> message .: "role"
        <*> message .: "requestId"
      String "session.leave"           -> LeaveSession   <$> message .: "sessionId" <*> message .: "participantId"
      String "puzzleIndex.set"         -> SetPuzzleIndex <$> message .: "sessionId" <*> message .: "puzzleIndex"
      String "round.start"             -> StartRound     <$> message .: "sessionId"
      String "round.stop"              -> StopRound      <$> message .: "sessionId"
      String "participant.input"       -> ParticipantInput
        <$> message .: "sessionId"
        <*> message .: "participantId"
        <*> message .: "input"
        <*> (posixSecondsToUTCTime <$> ((/) <$> message .: "timestamp" <*> pure 1000))
      String "solution.evaluated"      -> do
        taskId <- message .: "taskId"
        mError <- message .:? "error"
        mData <- message .:? "result"
        correct <- message .:? "correct"
        let result = getResult mError mData
                      where getResult (Just evalError) _  = Left evalError
                            getResult _ (Just resultJson) = Right $ encodeUtf8 resultJson
                            getResult _ _                 = Left "Malformed sandbox solution evaluation"
            correctness = fromMaybe Incorrect correct
        return $ EvaluatedSolution taskId result correctness

      String "puzzle.create"           -> CreatePuzzle   <$> message .: "puzzle"

      _ -> fail "Unrecognized incoming message"
  parseJSON _ = mzero
