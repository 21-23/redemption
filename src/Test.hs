{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Test
  ( Test(..)
  ) where

import Language.Haskell.TH.Syntax ( Type(ConT) )
import Database.Persist.TH
    ( mkPersist,
      mkPersistSettings,
      persistLowerCase,
      share,
      MkPersistSettings(mpsPrefixFields) )
import Database.Persist.MongoDB
    ( BackendKey(MongoKey), MongoContext )

import Data.Text (Text)

let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) { mpsPrefixFields = False }
 in share [mkPersist mongoSettings] [persistLowerCase|
Test json
    input Text
    expected Text
|]
