{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Session where

import Data.Map (Map)
import Database.MongoDB

import BSON
import Participant

data Session = Session
  { sessionId    :: ObjectId
  , gameMaster   :: Participant
  , participants :: Map String Participant
  }

instance ToBSON Session where
  toBSON Session {sessionId, gameMaster} = [
    "_id" =: sessionId,
    "gameMaster" =: toBSON gameMaster
    ]
