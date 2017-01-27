{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text
import qualified Network.WebSockets  as WS
import Data.Aeson

import State
import Message

app :: WS.ClientApp ()
app conn = do
  WS.sendTextData conn $ encode $ Envelope Messenger (ArnauxCheckin "state")
  forever $ do
    msg <- WS.receiveData conn
    liftIO $ Text.putStrLn msg

main :: IO ()
main =
  withSocketsDo $ WS.runClient "localhost" 3000 "/" app
