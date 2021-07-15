{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Round
    (Round(..)
    ) where

import           Database.Persist.MongoDB   (BackendKey (MongoKey),
                                             MongoContext)
import           Database.Persist.TH        (MkPersistSettings (mpsPrefixFields),
                                             mkPersist, mkPersistSettings,
                                             persistLowerCase, share)
import           Language.Haskell.TH.Syntax (Type (ConT))

import           Data.Map.Strict            (Map)
import           Data.Time.Clock            (UTCTime)

import           Participant                (ParticipantUid)
import           Solution                   (Solution)

let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) { mpsPrefixFields = False }
 in share [mkPersist mongoSettings] [persistLowerCase|
Round json
    puzzleIndex Int
    startTime UTCTime
    solutions (Map ParticipantUid Solution)
|]
