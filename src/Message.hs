{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Message where

import Control.Monad
import Data.Aeson
import Data.Semigroup

import Identity
import Participant
import Session
import Reference
import RoundPhase

data IncomingMessage
  = CreateSession Participant
  | JoinSession SessionRef Participant
  | LeaveSession SessionRef ParticipantRef
  | SetPuzzleIndex SessionRef Int
  | SetRoundPhase SessionRef RoundPhase
  | ParticipantInput SessionRef ParticipantRef String
  | EvaluatedSolution SessionRef ParticipantRef String Bool

data OutgoingMessage
  = ArnauxCheckin Identity
  | SessionCreated Session
  | ParticipantJoined SessionRef Participant
  | ParticipantLeft SessionRef ParticipantRef
  | PuzzleIndexChanged SessionRef Int
  | RoundPhaseChanged SessionRef RoundPhase
  | StartCountdownChanged SessionRef Int
  | RoundCountdownChanged SessionRef Int
  | ParticipantInputChanged SessionRef ParticipantRef Int
  | EvaluateSolution SessionRef ParticipantRef String
  | SolutionEvaluated SessionRef ParticipantRef String Bool

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

instance ToJSON OutgoingMessage where
  toJSON message = object $ ["name" .= toName message] <> toValue message
    where
      toValue (ArnauxCheckin identity) = ["identity" .= identity]
      toValue (SessionCreated session) = ["session" .= session]
      toValue (ParticipantJoined sessionId participant) =
        [ "sessionId" .= sessionId
        , "participant" .= participant
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


instance FromJSON IncomingMessage where
  parseJSON (Object message) = do
    name <- message .: "name"
    case name of
      String "session.create"          -> CreateSession  <$> message .: "gameMaster"
      String "session.join"            -> JoinSession    <$> message .: "sessionId" <*> message .: "participant"
      String "session.leave"           -> LeaveSession   <$> message .: "sessionId" <*> message .: "participantId"
      String "puzzleIndex.set"         -> SetPuzzleIndex <$> message .: "sessionId" <*> message .: "puzzleIndex"
      String "roundPhase.set"          -> SetRoundPhase  <$> message .: "sessionId" <*> message .: "roundPhase"
      String "participant.input"       -> ParticipantInput
        <$> message .: "sessionId"
        <*> message .: "participantId"
        <*> message .: "solution"
      String "solution.evaluated"      -> EvaluatedSolution
        <$> message .: "sessionId"
        <*> message .: "participantId"
        <*> message .: "solution"
        <*> message .: "result"

      _ -> fail "Unrecognized incoming message"
  parseJSON _ = mzero
