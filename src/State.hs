{-# LANGUAGE NamedFieldPuns #-}

module State where

import Data.Map

data Participant = Participant
  { id :: String
  , name :: String
  }

data Session = Session
  { gameMaster :: Participant
  , participants :: Map String Participant
  }

data State = State
  { sessions :: Map Int Session
  , sessionId :: Int }

createSession :: Participant -> State -> State
createSession gameMaster state@State{sessions, sessionId} =
  state { sessions = insert sessionId newSession sessions, sessionId = nextId  }
    where nextId = sessionId + 1
          newSession = Session { gameMaster, participants = empty }
