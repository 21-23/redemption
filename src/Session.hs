{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Session where

import Data.Text
import Data.Map (Map)
-- import qualified Data.Map as Map
import Database.MongoDB
-- import Data.Aeson
-- import Data.Sequence (Seq, ViewR(..), (|>))
-- import qualified Data.Sequence as Seq
-- import Data.Time.Clock
-- import Data.Foldable

import Language.Haskell.TH.Syntax
import Database.Persist.TH
-- import Database.Persist.MongoDB

import Participant
import RoundPhase
import Puzzle
import Round
-- import Solution (Solution(..))
import qualified Solution()

startCountdownTime :: Integer
startCountdownTime = 3

let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) { mpsGeneric = False, mpsPrefixFields = False }
 in share [mkPersist mongoSettings] [persistLowerCase|
Session json
    gameMaster Participant
    puzzles [Puzzle]
    participants (Map ParticipantUid Participant)
    rounds [Round]
    playerInput (Map ParticipantUid Text)
    puzzleIndex Int
    roundPhase RoundPhase
    startCountdown Int
    roundCountdown Int
|]

-- addParticipant :: Participant -> Session -> Session
-- addParticipant participant@Participant{participantId} session@Session{participants} =
--   session { participants = Map.insert participantId participant participants }
--
-- removeParticipant :: ParticipantRef -> Session -> Session
-- removeParticipant participantId session@Session{participants} =
--   session { participants = Map.delete participantId participants }
--
-- setPuzzleIndex :: Int -> Session -> Session
-- setPuzzleIndex newIndex session = session { puzzleIndex = newIndex }
--
-- getPuzzleIndex :: Session -> Int
-- getPuzzleIndex = puzzleIndex
--
-- addRound :: Round -> Session -> Session
-- addRound newRound session@Session{rounds} = session { rounds = rounds |> newRound }
--
-- setRoundPhase :: RoundPhase -> Session -> Session
-- setRoundPhase phase session = session { roundPhase = phase }
--
-- setStartCountdown :: Int -> Session -> Session
-- setStartCountdown value session = session { startCountdown = value }
--
-- getStartCountdown :: Session -> Int
-- getStartCountdown = startCountdown
--
-- setRoundCountdown :: Int -> Session -> Session
-- setRoundCountdown value session = session { roundCountdown = value }
--
-- getRoundCountdown :: Session -> Int
-- getRoundCountdown = roundCountdown
--
-- setParticipantInput :: ParticipantRef -> String -> Session -> Session
-- setParticipantInput participantId string session@Session{playerInput} =
--   session { playerInput = Map.insert participantId string playerInput }
--
-- addSolution :: ParticipantRef -> Solution -> Session -> Session
-- addSolution participantId solution session@Session{rounds} =
--   case Seq.viewr rounds of
--     EmptyR -> session
--     _ :> currentRound@Round{solutions} ->
--       session { rounds = Seq.update (Seq.length rounds - 1) updatedRound rounds }
--         where updatedRound = currentRound { solutions = Map.insert participantId solution solutions }
--
-- getLastRoundScore :: Session -> Map ParticipantRef NominalDiffTime
-- getLastRoundScore Session{participants, rounds} =
--   case Seq.viewr rounds of
--     EmptyR -> Map.empty
--     _ :> currentRound -> Map.map (getScore currentRound) participants
--       where getScore Round{startTime, solutions} Participant{participantId} =
--               case Map.lookup participantId solutions of
--                 Just Solution{time} -> diffUTCTime time startTime
--                 Nothing -> 5 -- 5 is a widely accepted fallback value
