{-# LANGUAGE TemplateHaskell #-}

module Game (Game(..), stringify, parseGame) where

import Control.Monad (MonadPlus, mzero)
import Data.Text (Text)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), Value(String))
import Database.Persist.TH ( derivePersistField )

data Game
  = CSSQuickDraw
  | LodashQuickDraw
  | JSQuickDraw
  deriving stock (Eq, Read, Show, Ord)

derivePersistField "Game"

stringify :: Game -> Text
stringify CSSQuickDraw    = "cssqd"
stringify LodashQuickDraw = "_qd"
stringify JSQuickDraw     = "jsqd"

parseGame :: (MonadPlus m) => Text -> m Game
parseGame "cssqd" = pure CSSQuickDraw
parseGame "_qd"   = pure LodashQuickDraw
parseGame "jsqd"  = pure JSQuickDraw
parseGame _       = mzero

instance ToJSON Game where
  toJSON = String . stringify

instance FromJSON Game where
  parseJSON (String string) = parseGame string
  parseJSON _               = mzero
