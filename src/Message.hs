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

data OutgoingMessage
  = ArnauxCheckin Identity
  | SessionCreated Session
  | ParticipantJoined SessionRef Participant
  | ParticipantLeft SessionRef ParticipantRef
  | PuzzleIndexChanged SessionRef Int
  | RoundPhaseChanged SessionRef RoundPhase

toName :: OutgoingMessage -> String
toName (ArnauxCheckin _) = "checkin"
toName (SessionCreated _) = "session.created"
toName (ParticipantJoined _ _) = "session.participant.joined"
toName (ParticipantLeft _ _) = "session.participant.left"
toName (PuzzleIndexChanged _ _) = "session.puzzleIndex.changed"
toName (RoundPhaseChanged _ _) = "session.roundPhase.changed"

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

instance FromJSON IncomingMessage where
  parseJSON (Object message) = do
    name <- message .: "name"
    case name of
      String "session.create" -> CreateSession <$> message .: "gameMaster"
      String "session.join"   -> JoinSession <$> message .: "sessionId" <*> message .: "participant"
      String "session.leave"  -> LeaveSession <$> message .: "sessionId" <*> message .: "participantId"
      String "session.puzzleIndex.set" -> SetPuzzleIndex <$> message .: "sessionId" <*> message .: "puzzleIndex"
      String "session.roundPhase.set"  -> SetRoundPhase <$> message .: "sessionId" <*> message .: "roundPhase"
      _ -> fail "Unrecognized incoming message"
  parseJSON _ = mzero
