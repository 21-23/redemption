{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Message where

import Control.Monad
import Data.Aeson
import Data.Semigroup
import Data.Time.Clock
import Data.Map

import Identity
import Participant
import Session
import RoundPhase
import Puzzle
import Role

data IncomingMessage
  = CreateSession Participant [PuzzleId]
  | JoinSession SessionId ParticipantUid
  | LeaveSession SessionId ParticipantUid
  | SetPuzzleIndex SessionId Int
  | SetRoundPhase SessionId RoundPhase
  | ParticipantInput SessionId ParticipantUid String
  | EvaluatedSolution SessionId ParticipantUid String Bool
  | CreatePuzzle Puzzle

data OutgoingMessage
  = ArnauxCheckin Identity
  | SessionCreated SessionId
  | ParticipantJoined SessionId ParticipantUid Role
  | ParticipantLeft SessionId ParticipantUid
  | PuzzleIndexChanged SessionId Int
  | RoundPhaseChanged SessionId RoundPhase
  | StartCountdownChanged SessionId Int
  | RoundCountdownChanged SessionId Int
  | ParticipantInputChanged SessionId ParticipantUid Int
  | EvaluateSolution SessionId ParticipantUid String
  | SolutionEvaluated SessionId ParticipantUid String Bool
  | RoundScore SessionId (Map ParticipantUid NominalDiffTime)
  | PuzzleCreated PuzzleId

toName :: OutgoingMessage -> String
toName ArnauxCheckin {}           = "checkin"
toName SessionCreated {}          = "session.created"
toName ParticipantJoined {}       = "participant.joined"
toName ParticipantLeft {}         = "participant.left"
toName PuzzleIndexChanged {}      = "puzzleIndex.changed"
toName RoundPhaseChanged {}       = "roundPhase.changed"
toName StartCountdownChanged {}   = "startCountdown.changed"
toName RoundCountdownChanged {}   = "roundCountdown.changed"
toName EvaluateSolution {}        = "solution.evaluate"
toName SolutionEvaluated {}       = "solution.evaluated"
toName ParticipantInputChanged {} = "participant.input.changed"
toName RoundScore {}              = "round.score"
toName PuzzleCreated {}           = "puzzle.created"

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
      toValue (PuzzleIndexChanged sessionId puzzleIndex) =
        [ "sessionId" .= sessionId
        , "puzzleIndex" .= puzzleIndex
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
      toValue (EvaluateSolution sessionId participantId solution) =
        [ "sessionId" .= sessionId
        , "participantId" .= participantId
        , "solution" .= solution
        ]
      toValue (SolutionEvaluated sessionId participantId solution correct) =
        [ "sessionId" .= sessionId
        , "participantId" .= participantId
        , "solution" .= solution
        , "correct" .= correct
        ]
      toValue (ParticipantInputChanged sessionId participantId len) =
        [ "sessionId" .= sessionId
        , "participantId" .= participantId
        , "length" .= len
        ]
      toValue (RoundScore sessionId score) =
        [ "sessionId" .= sessionId
        , "score" .= score
        ]
      toValue (PuzzleCreated puzzleId) = [ "puzzleId" .= puzzleId ]


instance FromJSON IncomingMessage where
  parseJSON (Object message) = do
    name <- message .: "name"
    case name of
      String "session.create"          -> CreateSession  <$> message .: "gameMaster" <*> message .: "puzzles"
      String "session.join"            -> JoinSession    <$> message .: "sessionId" <*> message .: "participantId"
      String "session.leave"           -> LeaveSession   <$> message .: "sessionId" <*> message .: "participantId"
      String "puzzleIndex.set"         -> SetPuzzleIndex <$> message .: "sessionId" <*> message .: "puzzleIndex"
      String "roundPhase.set"          -> SetRoundPhase  <$> message .: "sessionId" <*> message .: "roundPhase"
      String "participant.input"       -> ParticipantInput
        <$> message .: "sessionId"
        <*> message .: "participantId"
        <*> message .: "input"
      String "solution.evaluated"      -> EvaluatedSolution
        <$> message .: "sessionId"
        <*> message .: "participantId"
        <*> message .: "solution"
        <*> message .: "correct"
      String "puzzle.create"           -> CreatePuzzle   <$> message .: "puzzle"

      _ -> fail "Unrecognized incoming message"
  parseJSON _ = mzero
