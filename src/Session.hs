{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Session where

import Data.Map (Map)
import qualified Data.Map as Map
import Database.MongoDB
import Data.Aeson

import BSON
import Participant
import Reference

data Session = Session
  { sessionId    :: SessionRef
  , gameMaster   :: Participant
  , participants :: Map String Participant
  }

addParticipant :: Participant -> Session -> Session
addParticipant participant@Participant{participantId} session@Session{participants} =
  session { participants = Map.insert participantId participant participants }

removeParticipant :: ParticipantRef -> Session -> Session
removeParticipant participantId session@Session{participants} =
  session { participants = Map.delete participantId participants }

instance ToJSON Session where
  toJSON Session{sessionId, gameMaster, participants} = object
    [ "id" .= sessionId
    , "gameMaster" .= gameMaster
    , "participants" .= participants
    ]

instance ToBSON Session where
  toBSON Session {sessionId, gameMaster, participants} =
    [ "_id" =: sessionId
    , "gameMaster" =: toBSON gameMaster
    , "participants" =: toBSON <$> Map.elems participants
    ]
