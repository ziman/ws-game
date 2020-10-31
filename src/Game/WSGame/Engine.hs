{-# LANGUAGE AllowAmbiguousTypes #-}
module Game.WSGame.Engine
  ( Game(..), runGame
  , HasError(..)
  , Connection, send, close
  )
  where

import Data.Functor

import Control.Exception (SomeException)
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Control.Exception as Exception

import qualified Data.Aeson as Aeson

import qualified Network.WebSockets as WS

import qualified Game.WSGame.Game as Game

data Connection = Connection
  { id :: Int
  , ws :: WS.Connection
  }

instance Eq Connection where
  Connection{id=idL} == Connection{id=idR} = idL == idR

instance Ord Connection where
  compare Connection{id=idL} Connection{id=idR} = compare idL idR

instance Show Connection where
  show Connection{id} = "Connection " ++ show id

class HasError msg_S2C where
  s2cError :: String -> msg_S2C

-- this function can only return hard errors
recv :: Aeson.FromJSON msg_C2S => Connection -> IO (Either String msg_C2S)
recv Connection{ws} = do
  msgBS <- (Right <$> WS.receiveData ws)
    `Exception.catch` \e -> pure $ Left (show (e :: WS.ConnectionException))
  pure $ case Aeson.decode <$> msgBS of
    Left err -> Left $ "can't recv: " ++ err
    Right Nothing -> Left $ "can't decode: " ++ show msgBS
    Right (Just msg) -> Right msg

send :: Aeson.ToJSON msg_S2C => Connection -> msg_S2C -> IO ()
send Connection{ws} msg = WS.sendTextData ws (Aeson.encode msg)

-- this function returns exactly on (hard) error
playerLoop
  :: forall msg_S2C msg_C2S
  .  ( Aeson.FromJSON msg_C2S
     , Aeson.ToJSON msg_S2C
     , HasError msg_S2C
     )
  => Connection
  -> (msg_C2S -> IO (Either Game.Error ()))
  -> IO String
playerLoop connection handle =
  recv connection >>= \case
    Left err -> pure err  -- give up
    Right msg -> handle msg >>= \case
      Right () ->
        -- loop again
        playerLoop @msg_S2C connection handle

      Left (Game.SoftError msg) -> do
        -- send an error to the client but loop again
        send connection $ s2cError @msg_S2C msg
        playerLoop @msg_S2C connection handle

      Left (Game.HardError msg) ->
        -- give up
        pure msg

-- send close and receive all remaining messages
close :: forall msg_S2C. (Aeson.ToJSON msg_S2C, HasError msg_S2C) => Connection -> IO ()
close Connection{ws} =
  void $ forkIO $ do
    Exception.handle @SomeException (\_ -> pure ()) $ do
      WS.sendClose ws (Aeson.encode $ s2cError @msg_S2C "connection replaced")
      forever (void $ WS.receive ws)
    putStrLn $ "connection closed"

data Game st eff msg_C2S msg_S2C = Game
  { onMessage :: msg_C2S -> Game.GameM st eff Connection ()
  , onDeadPlayer :: Game.GameM st eff Connection ()
  , runEffect :: eff -> IO ()
  }

application
  :: forall msg_S2C msg_C2S st eff
  .  ( Aeson.FromJSON msg_C2S
     , Aeson.ToJSON msg_S2C
     , HasError msg_S2C
     )
  => TVar st -> TVar Int -> Game st eff msg_C2S msg_S2C -> WS.ServerApp
application tvState tvCounter Game{onMessage,onDeadPlayer,runEffect} pending = do
  wsConnection <- WS.acceptRequest pending
  WS.withPingThread wsConnection 30 (return ()) $ do
    connection <- atomically $ do
      nextId <- readTVar tvCounter
      writeTVar tvCounter (nextId + 1)
      pure Connection{id=nextId, ws=wsConnection}

    let env = Game.Env{connection, state=tvState}

    -- run the player until dead
    result <- playerLoop @msg_S2C connection (Game.runGameM env runEffect . onMessage)
      `Exception.catch`
        \e -> pure $ show (e :: SomeException)

    -- print reason of death
    putStrLn $ "connection " ++ show connection ++ " dead: " ++ result

    -- mark player as dead
    void $ Game.runGameM env runEffect onDeadPlayer

runGame
  :: forall msg_S2C msg_C2S st eff
  .  ( Aeson.FromJSON msg_C2S
     , Aeson.ToJSON msg_S2C
     , HasError msg_S2C
     )
  => String -> Int -> st -> Game st eff msg_C2S msg_S2C -> IO ()
runGame addr port initialState game = do
  tvState <- newTVarIO initialState
  tvCounter <- newTVarIO 0
  WS.runServer addr port
    $ application @msg_S2C tvState tvCounter game

