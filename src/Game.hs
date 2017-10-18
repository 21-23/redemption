{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Monad (mzero)

import Data.Aeson
import Database.Persist.TH

data Game
  = CSSQuickDraw
  | LodashQuickDraw deriving (Eq, Read, Show)

derivePersistField "Game"

instance ToJSON Game where
  toJSON CSSQuickDraw    = String "cssqd"
  toJSON LodashQuickDraw = String "_qd"

instance FromJSON Game where
  parseJSON (String "cssqd") = return CSSQuickDraw
  parseJSON (String "_qd")   = return LodashQuickDraw
  parseJSON _ = mzero
