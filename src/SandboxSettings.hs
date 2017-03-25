{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module SandboxSettings where

import Language.Haskell.TH.Syntax
import Database.Persist.TH
import Database.Persist.MongoDB

import NominalDiffTimePersistField()

import Data.Time.Clock (NominalDiffTime)

let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) {mpsGeneric = False}
 in share [mkPersist mongoSettings] [persistLowerCase|
SandboxSettings json
  timeout       NominalDiffTime
  reloadWorkers Bool
  refillWorkers Bool
|]
