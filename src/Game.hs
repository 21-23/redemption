{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Monad (mzero)

import Data.Aeson
import Database.Persist.TH

data Game
  = CSSQuickDraw
  | LodashQuickdraw deriving (Eq, Read, Show)

derivePersistField "Game"

instance ToJSON Game where
  toJSON CSSQuickDraw    = String "cssqd"
  toJSON LodashQuickdraw = String "lodashqd"

instance FromJSON Game where
  parseJSON (String "cssqd")    = return CSSQuickDraw
  parseJSON (String "lodashqd") = return LodashQuickdraw
  parseJSON _ = mzero
