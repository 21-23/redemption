module BSON where

import Database.MongoDB

class ToBSON a where
  toBSON :: a -> Document
