{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Session where

import Data.Map (Map)
import Database.MongoDB
import Data.Aeson

import BSON
import Participant

data Session = Session
  { sessionId    :: ObjectId
  , gameMaster   :: Participant
  , participants :: Map String Participant
  }

instance ToJSON Session where
  toJSON Session{sessionId, gameMaster, participants} = object
    [ "id" .= show sessionId
    , "gameMaster" .= gameMaster
    , "participants" .= participants
    ]

instance ToBSON Session where
  toBSON Session {sessionId, gameMaster} =
    [ "_id" =: sessionId
    , "gameMaster" =: toBSON gameMaster
    ]
