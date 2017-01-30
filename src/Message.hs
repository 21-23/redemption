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

data IncomingMessage
  = CreateSession Participant
  | JoinSession SessionRef Participant
  | LeaveSession SessionRef ParticipantRef

data OutgoingMessage
  = ArnauxCheckin Identity
  | SessionCreated Session
  | ParticipantJoined SessionRef Participant
  | ParticipantLeft SessionRef ParticipantRef

toName :: OutgoingMessage -> String
toName (ArnauxCheckin _) = "checkin"
toName (SessionCreated _) = "session.created"
toName (ParticipantJoined _ _) = "session.participant.joined"
toName (ParticipantLeft _ _) = "session.participant.left"

instance ToJSON OutgoingMessage where
  toJSON message = object $ ["name" .= toName message] <> toValue message
    where
      toValue (ArnauxCheckin identity) = ["identity" .= identity]
      toValue (SessionCreated session) = ["session" .= session]
      toValue (ParticipantJoined sessionId participant) =
        ["sessionId" .= sessionId
        , "participant" .= participant
        ]
      toValue (ParticipantLeft sessionId participantId) =
        ["sessionId" .= sessionId
        , "participantId" .= participantId
        ]

instance FromJSON IncomingMessage where
  parseJSON (Object message) = do
    name <- message .: "name"
    case name of
      String "session.create" -> CreateSession <$> message .: "gameMaster"
      String "session.join"   -> JoinSession <$> message .: "sessionId" <*> message .: "participant"
      String "session.leave"  -> LeaveSession <$> message .: "sessionId" <*> message .: "participantId"
      _ -> fail "Unrecognized incoming message"
  parseJSON _ = mzero
