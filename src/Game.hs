{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Monad (MonadPlus, mzero)
import Data.Text (Text)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), Value(String))
import Database.Persist.TH

data Game
  = CSSQuickDraw
  | LodashQuickDraw deriving (Eq, Read, Show, Ord)

derivePersistField "Game"

stringify :: Game -> Text
stringify CSSQuickDraw    = "cssqd"
stringify LodashQuickDraw = "_qd"

parseGame :: (MonadPlus m) => Text -> m Game
parseGame "cssqd" = pure CSSQuickDraw
parseGame "_qd"   = pure LodashQuickDraw
parseGame _       = mzero

instance ToJSON Game where
  toJSON = String . stringify

instance FromJSON Game where
  parseJSON (String string) = parseGame string
  parseJSON _               = mzero
