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
import Data.Time.Clock (NominalDiffTime)

import NominalDiffTimePersistField()
import SandboxSettings (SandboxSettings)

let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) { mpsGeneric = False, mpsPrefixFields = False }
 in share [mkPersist mongoSettings] [persistLowerCase|
Puzzle json
  name            Text
  input           Text
  expected        Text
  timeLimit       NominalDiffTime
  sandboxSettings SandboxSettings
|]
