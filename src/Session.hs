{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Session where

import Data.Map (Map)
import qualified Data.Map as Map
import Database.MongoDB
import Data.Aeson
import Data.Sequence (Seq, ViewR(..), (|>))
import qualified Data.Sequence as Seq

import BSON
import Participant
import Reference
import RoundPhase
import Puzzle
import Round (Round(Round, solutions))
import Solution (Solution)

startCountdownTime :: Integer
startCountdownTime = 3

data Session = Session
  { sessionId      :: SessionRef
  , gameMaster     :: Participant
  , puzzles        :: [Puzzle]
  , participants   :: Map ParticipantRef Participant
  , rounds         :: Seq Round
  , input          :: Map ParticipantRef String
  , puzzleIndex    :: Int
  , roundPhase     :: RoundPhase
  , startCountdown :: Int
  , roundCountdown :: Int
  }

addParticipant :: Participant -> Session -> Session
addParticipant participant@Participant{participantId} session@Session{participants} =
  session { participants = Map.insert participantId participant participants }

removeParticipant :: ParticipantRef -> Session -> Session
removeParticipant participantId session@Session{participants} =
  session { participants = Map.delete participantId participants }

setPuzzleIndex :: Int -> Session -> Session
setPuzzleIndex newIndex session = session { puzzleIndex = newIndex }

getPuzzleIndex :: Session -> Int
getPuzzleIndex = puzzleIndex

addRound :: Round -> Session -> Session
addRound newRound session@Session{rounds} = session { rounds = rounds |> newRound }

setRoundPhase :: RoundPhase -> Session -> Session
setRoundPhase phase session = session { roundPhase = phase }

setStartCountdown :: Int -> Session -> Session
setStartCountdown value session = session { startCountdown = value }

getStartCountdown :: Session -> Int
getStartCountdown = startCountdown

setRoundCountdown :: Int -> Session -> Session
setRoundCountdown value session = session { roundCountdown = value }

getRoundCountdown :: Session -> Int
getRoundCountdown = roundCountdown

setParticipantInput :: ParticipantRef -> String -> Session -> Session
setParticipantInput participantId string session@Session{input} =
  session { input = Map.insert participantId string input }

addSolution :: ParticipantRef -> Solution -> Session -> Session
addSolution participantId solution session@Session{rounds} =
  case Seq.viewr rounds of
    EmptyR -> session
    _ :> currentRound@Round{solutions} ->
      session { rounds = Seq.update (Seq.length rounds - 1) updatedRound rounds }
        where updatedRound = currentRound { solutions = Map.insert participantId solution solutions }

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
