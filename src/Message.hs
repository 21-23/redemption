{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Message where

import Control.Monad
import Data.Aeson
import Data.Semigroup
import Data.Time.Clock
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq

import Identity
import Participant
import Session
import RoundPhase
import Puzzle
import Role

data IncomingMessage
  = CreateSession ParticipantUid [PuzzleId]
  | JoinSession SessionId ParticipantUid
  | LeaveSession SessionId ParticipantUid
  | SetPuzzleIndex SessionId Int
  | StartRound SessionId
  | StopRound SessionId
  | ParticipantInput SessionId ParticipantUid String
  | EvaluatedSolution SessionId ParticipantUid String Bool
  | CreatePuzzle Puzzle

data OutgoingMessage
  = ArnauxCheckin Identity
  | SessionCreated SessionId
  | ParticipantJoined SessionId ParticipantUid Role
  | ParticipantLeft SessionId ParticipantUid
  | PuzzleChanged SessionId Int Puzzle
  | RoundPhaseChanged SessionId RoundPhase
  | SetSandbox Puzzle
  | ResetSandbox
  | StartCountdownChanged SessionId Int
  | RoundCountdownChanged SessionId Int
  | RoundPuzzle SessionId Puzzle
  | ParticipantInputChanged SessionId ParticipantUid Int
  | EvaluateSolution SessionId ParticipantUid String
  | SolutionEvaluated SessionId ParticipantUid String Bool
  | RoundScore SessionId (Map ParticipantUid NominalDiffTime)
  | PuzzleCreated PuzzleId
  | PlayerSessionState SessionId ParticipantUid Session

toName :: OutgoingMessage -> String
toName ArnauxCheckin {}           = "checkin"
toName SessionCreated {}          = "session.created"
toName ParticipantJoined {}       = "participant.joined"
toName ParticipantLeft {}         = "participant.left"
toName PuzzleChanged {}           = "puzzle.changed"
toName RoundPhaseChanged {}       = "roundPhase.changed"
toName SetSandbox {}              = "sandbox.set"
toName ResetSandbox {}            = "sandbox.reset"
toName StartCountdownChanged {}   = "startCountdown.changed"
toName RoundCountdownChanged {}   = "roundCountdown.changed"
toName RoundPuzzle {}             = "puzzle"
toName EvaluateSolution {}        = "solution.evaluate"
toName SolutionEvaluated {}       = "solution.evaluated"
toName ParticipantInputChanged {} = "participant.input.changed"
toName RoundScore {}              = "round.score"
toName PuzzleCreated {}           = "puzzle.created"
toName PlayerSessionState {}      = "player.sessionState"

instance ToJSON OutgoingMessage where
  toJSON message = object $ ["name" .= toName message] <> toValue message
    where
      toValue (ArnauxCheckin identity) = ["identity" .= identity]
      toValue (SessionCreated sessionId) = ["sessionId" .= sessionId]
      toValue (ParticipantJoined sessionId participantId role) =
        [ "sessionId" .= sessionId
        , "participantId" .= participantId
        , "role" .= role
        ]
      toValue (ParticipantLeft sessionId participantId) =
        [ "sessionId" .= sessionId
        , "participantId" .= participantId
        ]
      toValue (PuzzleChanged sessionId puzzleIndex puzzle) =
        [ "sessionId" .= sessionId
        , "puzzleIndex" .= puzzleIndex
        , "puzzleName" .= name puzzle
        , "timeLimit" .= timeLimit puzzle
        ]
      toValue (RoundPhaseChanged sessionId phase) =
        [ "sessionId" .= sessionId
        , "roundPhase" .= phase
        ]
      toValue (StartCountdownChanged sessionId value) =
        [ "sessionId" .= sessionId
        , "startCountdown" .= value
        ]
      toValue (RoundCountdownChanged sessionId value) =
        [ "sessionId" .= sessionId
        , "roundCountdown" .= value
        ]
      toValue (SetSandbox puzzle) =
        [ "input" .= input puzzle
        , "settings" .= sandboxSettings puzzle
        ]
      toValue ResetSandbox = []
      toValue (RoundPuzzle sessionId puzzle) =
        [ "sessionId" .= sessionId
        , "input" .= input puzzle
        , "expected" .= expected puzzle
        ]
      toValue (EvaluateSolution sessionId participantId solution) =
        [ "sessionId" .= sessionId
        , "participantId" .= participantId
        , "solution" .= solution
        ]
      toValue (SolutionEvaluated sessionId participantId solution correct) =
        [ "sessionId" .= sessionId
        , "participantId" .= participantId
        , "solution" .= solution
        , "correct" .= correct
        ]
      toValue (ParticipantInputChanged sessionId participantId len) =
        [ "sessionId" .= sessionId
        , "participantId" .= participantId
        , "length" .= len
        ]
      toValue (RoundScore sessionId score) =
        [ "sessionId" .= sessionId
        , "score" .= score
        ]
      toValue (PuzzleCreated puzzleId) = [ "puzzleId" .= puzzleId ]
      toValue (PlayerSessionState sessionId playerId session) =
        [ "sessionId" .= sessionId
        , "participantId" .= playerId
        , "puzzleIndex" .= puzzleIndex session
        , "puzzleCount" .= (length $ puzzles session)
        , "puzzle" .=  Seq.lookup (puzzleIndex session) (puzzles session)
        , "roundPhase" .= roundPhase session
        , "roundCountdown" .= roundCountdown session
        , "startCountdown" .= startCountdown session
        , "playerInput" .= fromMaybe "" (Map.lookup playerId $ playerInput session)
        ]

instance FromJSON IncomingMessage where
  parseJSON (Object message) = do
    name <- message .: "name"
    case name of
      String "session.create"          -> CreateSession  <$> message .: "gameMasterId" <*> message .: "puzzles"
      String "session.join"            -> JoinSession    <$> message .: "sessionId" <*> message .: "participantId"
      String "session.leave"           -> LeaveSession   <$> message .: "sessionId" <*> message .: "participantId"
      String "puzzleIndex.set"         -> SetPuzzleIndex <$> message .: "sessionId" <*> message .: "puzzleIndex"
      String "round.start"             -> StartRound     <$> message .: "sessionId"
      String "round.stop"              -> StopRound      <$> message .: "sessionId"
      String "participant.input"       -> ParticipantInput
        <$> message .: "sessionId"
        <*> message .: "participantId"
        <*> message .: "input"
      String "solution.evaluated"      -> EvaluatedSolution
        <$> message .: "sessionId"
        <*> message .: "participantId"
        <*> message .: "solution"
        <*> message .: "correct"
      String "puzzle.create"           -> CreatePuzzle   <$> message .: "puzzle"

      _ -> fail "Unrecognized incoming message"
  parseJSON _ = mzero
