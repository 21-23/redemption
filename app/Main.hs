{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Control.Concurrent
import           Control.Monad          (forever)
import           Control.Monad.IO.Class (MonadIO)
import           Network.Socket         (withSocketsDo)
import qualified Network.WebSockets     as WS
import Data.Aeson
import Database.MongoDB

import BSON
import State (State)
import qualified State
import Message
import Identity
import Envelope

updateState :: MVar State -> (State -> State) -> IO ()
updateState stateVar action = do
  state <- takeMVar stateVar
  putMVar stateVar $ action state

doMongoDBAction :: (MonadIO m) => Pipe -> Action m a -> m a
doMongoDBAction pipe = access pipe master "redemption-test"

sendMessage :: WS.Connection -> Identity -> OutgoingMessage -> IO ()
sendMessage connection to message =
  WS.sendTextData connection $ encode $ Envelope to message

app :: MVar State -> Pipe -> WS.ClientApp ()
app stateVar dbPipe connection = do
  WS.sendTextData connection $ encode $ Envelope Messenger (ArnauxCheckin StateService)
  let run = doMongoDBAction dbPipe
  forever $ do
    string <- WS.receiveData connection
    case eitherDecode string :: Either String (Envelope IncomingMessage) of
      Right Envelope {message} -> case message of
        CreateSession gameMaster -> do
          oid <- genObjectId
          let session = State.createSession oid gameMaster
          updateState stateVar $ State.addSession session
          _ <- run $ insert "sessions" $ toBSON session
          sendMessage connection FrontService $ SessionCreated session
      Left err -> putStrLn err

main :: IO ()
main = do
  stateVar <- newMVar State.empty
  pipe <- connect (host "127.0.0.1")
  withSocketsDo $ WS.runClient "localhost" 3000 "/" $ app stateVar pipe
