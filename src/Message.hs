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
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.UUID (UUID)
import Data.ByteString.Lazy (ByteString)

import Identity
import Participant
import Session
import RoundPhase
import Puzzle
import Role
import UUIDPersistField()
import UUIDAeson()
import PlayerRoundData (PlayerRoundData)

data IncomingMessage
  = CreateSession ParticipantUid SessionAlias [PuzzleId]
  | JoinSession SessionAlias ParticipantUid
  | LeaveSession SessionAlias ParticipantUid
  | SetPuzzleIndex SessionAlias Int
  | StartRound SessionAlias
  | StopRound SessionAlias
  | ParticipantInput SessionAlias ParticipantUid Text UTCTime
  | EvaluatedSolution UUID (Either Text ByteString)
  | CreatePuzzle Puzzle

data OutgoingMessage
  = ArnauxCheckin Identity
  | SessionCreated SessionAlias
  | ParticipantJoined SessionAlias ParticipantUid Role
  | ParticipantLeft SessionAlias ParticipantUid
  | PuzzleChanged SessionAlias Int Puzzle
  | RoundPhaseChanged SessionAlias RoundPhase
  | SetSandbox Puzzle
  | ResetSandbox
  | StartCountdownChanged SessionAlias Int
  | RoundCountdownChanged SessionAlias Int
  | RoundPuzzle SessionAlias Puzzle
  | EvaluateSolution UUID Text
  | SolutionEvaluated SessionAlias ParticipantUid (Either Text ByteString) NominalDiffTime Int Bool
  | Score SessionAlias [PlayerRoundData]
  | PuzzleCreated PuzzleId
  | PlayerSessionState SessionAlias ParticipantUid Session
  | GameMasterSessionState SessionAlias ParticipantUid Session

toName :: OutgoingMessage -> Text
toName ArnauxCheckin {}           = "checkin"
toName SessionCreated {}          = "session.created"
toName ParticipantJoined {}       = "participant.joined"
toName ParticipantLeft {}         = "participant.left"
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
      toValue (ParticipantJoined sessionId participantId role) =
        [ "sessionId" .= sessionId
        , "participantId" .= participantId
        , "role" .= role
        ]
      toValue (ParticipantLeft sessionId participantId) =
        [ "sessionId" .= sessionId
        , "participantId" .= participantId
        ]
      toValue (PuzzleChanged sessionId puzzleIndex puzzle) =
        [ "sessionId" .= sessionId
        , "puzzleIndex" .= puzzleIndex
        , "puzzleName" .= name puzzle
        , "timeLimit" .= timeLimit puzzle
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
        , "settings" .= sandboxSettings puzzle
        ]
      toValue ResetSandbox = []
      toValue (RoundPuzzle sessionId puzzle) =
        [ "sessionId" .= sessionId
        , "input" .= input puzzle
        , "expected" .= expected puzzle
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
        , "puzzleCount" .= (length $ puzzles session)
        , "puzzle" .=  getPuzzleForSessionState session
        , "roundPhase" .= roundPhase session
        , "roundCountdown" .= roundCountdown session
        , "startCountdown" .= startCountdown session
        , "playerInput" .= fromMaybe "" (Map.lookup participantId $ playerInput session)
        ]
      toValue (GameMasterSessionState sessionId participantId session) =
        [ "sessionId" .= sessionId
        , "participantId" .= participantId
        , "puzzleIndex" .= puzzleIndex session
        , "puzzleCount" .= (length $ puzzles session)
        , "puzzle" .= getPuzzleForSessionState session
        , "roundPhase" .= roundPhase session
        , "roundCountdown" .= roundCountdown session
        , "startCountdown" .= startCountdown session
        , "players" .= getPlayerRoundData session
        ]

getPuzzleForSessionState :: Session -> Maybe Puzzle
getPuzzleForSessionState session =
  let getPuzzle = lookupPuzzle (puzzleIndex session) session
   in case roundPhase session of
        InProgress -> getPuzzle
        End        -> getPuzzle
        _          -> Nothing

instance FromJSON IncomingMessage where
  parseJSON (Object message) = do
    name <- message .: "name"
    case name of
      String "session.create"          -> CreateSession
        <$> message .: "gameMasterId"
        <*> message .: "alias"
        <*> message .: "puzzles"
      String "session.join"            -> JoinSession    <$> message .: "sessionId" <*> message .: "participantId"
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
        let result = getResult mError mData
                      where getResult (Just evalError) _  = Left evalError
                            getResult _ (Just resultJson) = Right $ encodeUtf8 resultJson
                            getResult _ _                 = Left "Malformed sandbox solution evaluation"
        return $ EvaluatedSolution taskId result

      String "puzzle.create"           -> CreatePuzzle   <$> message .: "puzzle"

      _ -> fail "Unrecognized incoming message"
  parseJSON _ = mzero
