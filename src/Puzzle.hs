{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Puzzle where

import Control.Monad
import Data.Time.Clock (NominalDiffTime)
import Data.Aeson
import Database.MongoDB

import BSON
import Reference
import SandboxSettings

data Puzzle = Puzzle
  { puzzleId        :: Maybe PuzzleRef
  , name            :: String
  , input           :: String
  , expected        :: String
  , timeLimit       :: NominalDiffTime
  , sandboxSettings :: SandboxSettings
  }

instance FromJSON Puzzle where
  parseJSON (Object puzzle) = Puzzle
    <$> puzzle .:? "id"
    <*> puzzle .: "name"
    <*> puzzle .: "input"
    <*> puzzle .: "expected"
    <*> puzzle .: "timeLimit"
    <*> puzzle .: "sandboxSettings"
  parseJSON _ = mzero

instance ToJSON Puzzle where
  toJSON Puzzle {puzzleId, name, input, expected, sandboxSettings} =
    object [
      "id" .= puzzleId,
      "name" .= name,
      "input" .= input,
      "expected" .= expected,
      "sandboxSettings" .= sandboxSettings
    ]

instance ToBSON Puzzle where
  toBSON Puzzle {puzzleId, name, input, expected, sandboxSettings} =
    [ "_id" =: puzzleId
    , "name" =: name
    , "input" =: input
    , "expected" =: expected
    , "sandboxSettings" =: toBSON sandboxSettings
    ]
