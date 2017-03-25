{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Round where

import Language.Haskell.TH.Syntax
import Database.Persist.TH
import Database.Persist.MongoDB

import Data.Time.Clock
import Data.Map (Map)

import Participant
import Solution

let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) {mpsGeneric = False}
 in share [mkPersist mongoSettings] [persistLowerCase|
Round json
    puzzleIndex Int
    startTime UTCTime
    solutions (Map ParticipantUid Solution)
|]
