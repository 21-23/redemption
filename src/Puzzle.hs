{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Puzzle
  ( Puzzle(..)
  , PuzzleId
  , toSimpleJSON
  ) where

import           Database.Persist.MongoDB    (BackendKey (MongoKey),
                                              MongoContext)
import           Database.Persist.TH         (MkPersistSettings (mpsPrefixFields),
                                              mkPersist, mkPersistSettings,
                                              persistLowerCase, share)
import           Language.Haskell.TH.Syntax  (Type (..))

import           Data.Text                   (Text)

import           Data.Aeson                  (Value, object, (.=))
import           NominalDiffTimePersistField ()
import           Test                        (Test)

import           Game                        (Game (..))
import           PuzzleOptions               (PuzzleOptions (..))
import qualified PuzzleOptions
import           SandboxSettings             (SandboxSettings)

let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) { mpsPrefixFields = False }
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
  , "options" .= PuzzleOptions.toSimpleJSON options
  ]
