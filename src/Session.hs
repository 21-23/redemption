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
import RoundPhase
import Puzzle

data Session = Session
  { sessionId    :: SessionRef
  , gameMaster   :: Participant
  , participants :: Map String Participant
  , puzzles      :: [Puzzle]
  , puzzleIndex  :: Int
  , roundPhase   :: RoundPhase
  }

addParticipant :: Participant -> Session -> Session
addParticipant participant@Participant{participantId} session@Session{participants} =
  session { participants = Map.insert participantId participant participants }

removeParticipant :: ParticipantRef -> Session -> Session
removeParticipant participantId session@Session{participants} =
  session { participants = Map.delete participantId participants }

setPuzzleIndex :: Int -> Session -> Session
setPuzzleIndex newIndex session = session { puzzleIndex = newIndex }

setRoundPhase :: RoundPhase -> Session -> Session
setRoundPhase phase session = session { roundPhase = phase }

instance ToJSON Session where
  toJSON Session{sessionId, gameMaster, participants, puzzleIndex, roundPhase} = object
    [ "id" .= sessionId
    , "gameMaster" .= gameMaster
    , "participants" .= participants
    , "puzzleIndex" .= puzzleIndex
    , "roundPhase" .= roundPhase
    ]

instance ToBSON Session where
  toBSON Session {sessionId, gameMaster, participants, puzzleIndex, roundPhase} =
    [ "_id" =: sessionId
    , "gameMaster" =: toBSON gameMaster
    , "participants" =: toBSON <$> Map.elems participants
    , "puzzleIndex" =: puzzleIndex
    , "roundPhase" =: show roundPhase
    ]
