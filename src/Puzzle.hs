module Puzzle where

import Reference

data Puzzle = Puzzle
  { puzzleId  :: PuzzleRef
  , name      :: String
  , inputData :: String
  }
