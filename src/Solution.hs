{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Solution where

import Language.Haskell.TH.Syntax
import Database.Persist.TH
import Database.Persist.MongoDB

import Data.Text
import Data.Time.Clock

let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) { mpsGeneric = False, mpsPrefixFields = False }
 in share [mkPersist mongoSettings] [persistLowerCase|
Solution json
    code Text
    time UTCTime
|]
