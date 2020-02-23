{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EventInjector
  ( server
  , API
  )
where

import           Data.Pool                        (withResource)
import           Database.PostgreSQL.Simple       (execute, withTransaction)
import           Database.PostgreSQL.Simple.SqlQQ
import           EventInjector.Types
import           RIO
import           Servant                          as S
import           Servant.Auth.Server
import           Types

discoveredEmployeeEvent
  :: DiscoveredEmployee -> App NoContent
discoveredEmployeeEvent e = do
  (AppContext p _) <- ask
  liftIO $ insertEvent p e
  where
    insertEvent pool payload = do
      let eventName :: Text = "DISCOVERED_EMPLOYEE"
      let version :: Text = "v1"
      withResource pool $ \conn -> withTransaction conn $ do
        _ <- execute
          conn
          [sql|
            INSERT INTO store.store (name, version, payload)
            VALUES (?, ?, ?)
          |] (eventName, version, payload)
        return NoContent

discoveredProjectEvent
  :: DiscoveredProject -> App NoContent
discoveredProjectEvent e = do
  (AppContext p _) <- ask
  liftIO $ insertEvent p e
  where
    insertEvent pool payload = do
      let eventName :: Text = "DISCOVERED_PROJECT"
      let version :: Text = "v1"
      withResource pool $ \conn -> withTransaction conn $ do
        _ <- execute
          conn
          [sql|
            INSERT INTO store.store (name, version, payload)
            VALUES (?, ?, ?)
          |] (eventName, version, payload)
        return NoContent


receivedStandupUpdateEvent
  :: ReceivedStandupUpdate -> App NoContent
receivedStandupUpdateEvent e = do
  (AppContext p _) <- ask
  liftIO $ insertEvent p e
  where
    insertEvent pool payload = do
      let eventName :: Text = "RECEIVED_STANDUP_UPDATE"
      let version :: Text = "v1"
      withResource pool $ \conn -> withTransaction conn $ do
        _ <- execute
          conn
          [sql|
            INSERT INTO store.store (name, version, payload)
            VALUES (?, ?, ?)
          |] (eventName, version, payload)
        return NoContent

server :: ServerT (API auths) App
server (Authenticated _) = discoveredEmployeeEvent
    :<|> discoveredProjectEvent
    :<|> receivedStandupUpdateEvent
-- FIXME: There have to be a better way
server _ = (\_ -> throwM err403) :<|> (\_ -> throwM err403) :<|> (\_ -> throwM err403)
