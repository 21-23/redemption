module Round where

import Data.Time.Clock
import Data.Map

import Reference
import Solution

data Round = Round
  { puzzleIndex :: Int
  , startTime   :: UTCTime
  , solutions   :: Map ParticipantRef Solution
  }
