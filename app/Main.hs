{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Control.Concurrent
import           Control.Monad       (forever, unless)
import           Network.Socket      (withSocketsDo)
import qualified Network.WebSockets  as WS
import Data.Aeson
import Database.MongoDB

import BSON
import State (State)
import qualified State
import Session
import Message
import Envelope

app :: MVar State -> Pipe -> WS.ClientApp ()
app stateVar dbPipe conn = do
  WS.sendTextData conn $ encode $ Envelope Messenger (ArnauxCheckin "state")
  forever $ do
    string <- WS.receiveData conn
    let run = access dbPipe master "redemption-test"
    case eitherDecode string :: Either String Envelope of
      Right Envelope {message} -> case message of
        CreateSession gameMaster -> do
          oid <- genObjectId
          let session = State.createSession oid gameMaster
          state <- takeMVar stateVar
          putMVar stateVar $ State.addSession session state
          run $ insert "sessions" $ toBSON session
          return ()
      Left err -> do
        putStrLn err
        return ()

main :: IO ()
main = do
  stateVar <- newMVar State.empty
  pipe <- connect (host "127.0.0.1")
  withSocketsDo $ WS.runClient "localhost" 3000 "/" $ app stateVar pipe
