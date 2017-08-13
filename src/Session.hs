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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (|>), ViewR(..))
import qualified Data.Sequence as Seq
import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)

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
import SolutionCorrectness (SolutionCorrectness(Correct))

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
    puzzleIndex    Int Maybe
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

setPuzzleIndex :: Maybe Int -> Session -> Session
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

getSolution :: ParticipantUid -> Session -> Maybe Solution
getSolution participantId session = do
  Round{solutions} <- getCurrentRound session
  Map.lookup participantId solutions

hasCorrectSolution :: ParticipantUid -> Session -> Bool
hasCorrectSolution participantId session = fromMaybe False $ do
  Solution{correct} <- getSolution participantId session
  return $ correct == Correct

getPlayerAggregateScore :: ParticipantUid -> Session -> Maybe NominalDiffTime
getPlayerAggregateScore participantId session@Session{rounds} =
  if Seq.null rounds
    then Nothing
    else Just $ foldl (\acc (Round puzzleIndex _ solutions) ->
                  let puzzle = lookupPuzzle puzzleIndex session
                      defaultTime = fromMaybe 0 $ timeLimit <$> puzzle
                   in acc + case Map.lookup participantId solutions of
                             Just (Solution _ time Correct) -> time
                             Just _                         -> defaultTime
                             Nothing                        -> defaultTime
                  ) 0 rounds

getPlayerRoundData :: Session -> [PlayerRoundData]
getPlayerRoundData session@Session{participants, playerInput, gameMasterId} =
  sortBy (comparing aggregateScore) $ map f $ filter ((/= gameMasterId) . fst) $ Map.toList participants
    where f (playerId, _) = PlayerRoundData
                        { participantId = playerId
                        , inputLength = Text.length $ fromMaybe "" $ Map.lookup playerId playerInput
                        , solution = (solutions <$> getCurrentRound session) >>= Map.lookup playerId
                        , aggregateScore = getPlayerAggregateScore playerId session
                        }
