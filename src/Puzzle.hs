{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Puzzle where

import Language.Haskell.TH.Syntax (Type(..))
import Database.Persist.TH
import Database.Persist.MongoDB

import Data.Text

import NominalDiffTimePersistField()
import Test (Test)
import Data.Aeson (Value, object, (.=))
import Data.Monoid ((<>))

import Game (Game(..))
import PuzzleOptions (PuzzleOptions(..))
import SandboxSettings (SandboxSettings)

let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) { mpsGeneric = False, mpsPrefixFields = False }
 in share [mkPersist mongoSettings] [persistLowerCase|
Puzzle json
  game            Game
  name            Text
  input           Text
  expected        Text
  hidden         [Test]
  options         PuzzleOptions
  sandboxSettings SandboxSettings
|]

toSimpleJSON :: Puzzle -> Value
toSimpleJSON Puzzle { name, input, expected, options } = object
  [ "name" .= name
  , "input" .= input
  , "expected" .= expected
  , "options" .= object (["timeLimit" .= timeLimit options] <> bannedCharacters)
  ]
    where
      bannedCharacters = case PuzzleOptions.bannedCharacters options of
                           Just characterList -> [ "bannedCharacters" .= characterList ]
                           Nothing            -> []
