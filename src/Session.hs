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

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Map (Map)
import qualified Data.Map as Map
-- import Data.Map.Merge.Strict (merge, mapMissing)
import Data.Aeson (Value, decode)
import Data.Sequence (Seq, (|>), ViewR(..))
import qualified Data.Sequence as Seq
import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)
import Data.Maybe (fromMaybe)

import Language.Haskell.TH.Syntax (Type(..))
import Database.Persist.TH
import Database.Persist.MongoDB

import Participant
import RoundPhase
import Puzzle
import Round (Round(Round, solutions))
import qualified Round
import Role (Role)
import qualified Role
import SequencePersistField()
import Solution (Solution(..))
import qualified Solution()
import PlayerRoundData (PlayerRoundData(..))

startCountdownTime :: Integer
startCountdownTime = 3

type SessionAlias = Text

let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) { mpsGeneric = False, mpsPrefixFields = False }
 in share [mkPersist mongoSettings] [persistLowerCase|
Session json
    gameMasterId   ParticipantUid
    puzzles        (Seq Puzzle)
    participants   (Map ParticipantUid Participant)
    rounds         (Seq Round)
    playerInput    (Map ParticipantUid Text)
    puzzleIndex    Int
    roundPhase     RoundPhase
    startCountdown Int
    roundCountdown Int
    alias          SessionAlias
|]

addParticipant :: Participant -> Session -> Session
addParticipant participant@Participant{uid} session@Session{participants} =
  session { participants = Map.insert uid participant participants }

getParticipantRole :: ParticipantUid -> Session -> Role
getParticipantRole participantId session =
  if participantId == gameMasterId session
    then Role.GameMaster
    else Role.Player

removeParticipant :: ParticipantUid -> Session -> Session
removeParticipant participantId session@Session{participants} =
  session { participants = Map.delete participantId participants }

setPuzzleIndex :: Int -> Session -> Session
setPuzzleIndex newIndex session = session { puzzleIndex = newIndex }

lookupPuzzle :: Int -> Session -> Maybe Puzzle
lookupPuzzle index session = Seq.lookup index (puzzles session)

addRound :: Round -> Session -> Session
addRound newRound session@Session{rounds} = session { rounds = rounds |> newRound }

getCurrentRound :: Session -> Maybe Round
getCurrentRound Session{rounds} =
  case Seq.viewr rounds of
    EmptyR -> Nothing
    _ :> currentRound -> Just currentRound

setRoundPhase :: RoundPhase -> Session -> Session
setRoundPhase phase session = session { roundPhase = phase }

setStartCountdown :: Int -> Session -> Session
setStartCountdown value session = session { startCountdown = value }

getStartCountdown :: Session -> Int
getStartCountdown = startCountdown

setRoundCountdown :: Int -> Session -> Session
setRoundCountdown value session = session { roundCountdown = value }

setParticipantInput :: ParticipantUid -> Text -> Session -> Session
setParticipantInput participantId string session@Session{playerInput} =
  session { playerInput = Map.insert participantId string playerInput }

isSolutionCorrect :: Value -> Session -> Bool
isSolutionCorrect solutionData session =
  case lookupPuzzle (puzzleIndex session) session of
    Just puzzle ->
      case decode $ encodeUtf8 $ fromStrict $ expected puzzle of
        Just expectedData -> solutionData == expectedData
        Nothing -> False
    Nothing -> False

getSolutionTime :: UTCTime -> Session -> NominalDiffTime
getSolutionTime time session =
  case getCurrentRound session of
    Just currentRound -> diffUTCTime time $ Round.startTime currentRound
    Nothing -> 0


addSolution :: ParticipantUid -> Solution -> Session -> Session
addSolution participantId solution session@Session{rounds} =
  case getCurrentRound session of
    Nothing -> session
    Just currentRound@Round{solutions} ->
      session { rounds = Seq.update (Seq.length rounds - 1) updatedRound rounds }
        where updatedRound = currentRound { solutions = Map.insert participantId solution solutions }

getPlayerAggregateScore :: ParticipantUid -> Session -> Maybe NominalDiffTime
getPlayerAggregateScore participantId session@Session{rounds} =
  if Seq.null rounds
    then Nothing
    else Just $ foldl (\acc (Round puzzleIndex _ solutions) ->
                  let puzzle = lookupPuzzle puzzleIndex session
                      defaultTime = fromMaybe 0 $ timeLimit <$> puzzle
                   in acc + case Map.lookup participantId solutions of
                             Just (Solution _ time True)  -> time
                             Just (Solution _ _    False) -> defaultTime
                             Nothing                      -> defaultTime
                  ) 0 rounds

getPlayerRoundData :: Session -> [PlayerRoundData]
getPlayerRoundData session@Session{participants, playerInput, gameMasterId} =
  map f $ filter ((/= gameMasterId) . fst) $ Map.toList participants
    where f (playerId, _) = PlayerRoundData
                        { participantId = playerId
                        , inputLength = Text.length $ fromMaybe "" $ Map.lookup playerId playerInput
                        , solution = (solutions <$> getCurrentRound session) >>= Map.lookup playerId
                        , aggregateScore = getPlayerAggregateScore playerId session
                        }
