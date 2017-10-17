{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module PuzzleOptions where

import Language.Haskell.TH.Syntax (Type(..))
import Database.Persist.TH
import Database.Persist.MongoDB

import Data.Time.Clock (NominalDiffTime)

import SandboxSettings (SandboxSettings)

let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) { mpsGeneric = False, mpsPrefixFields = False }
 in share [mkPersist mongoSettings] [persistLowerCase|
PuzzleOptions json
  timeLimit         NominalDiffTime
  bannedCharacters [String] Maybe
  sandboxSettings   SandboxSettings
|]
