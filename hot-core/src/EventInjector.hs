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
  :: DiscoveredEmployeeEvent -> App NoContent
discoveredEmployeeEvent e = do
  (AppContext p _) <- ask
  liftIO $ insertEvent p e
  where
    insertEvent pool (DiscoveredEmployeeEvent event) = do
      let eventName :: Text = "DISCOVERED_EMPLOYEE"
      withResource pool $ \conn -> withTransaction conn $ do
        _ <- execute
          conn
          [sql|
            INSERT INTO store.store (name, version, payload)
            VALUES (?, ?, ?)
          |] (eventName, version event, payload event)
        return NoContent

discoveredProjectEvent
  :: DiscoveredProjectEvent -> App NoContent
discoveredProjectEvent e = do
  (AppContext p _) <- ask
  liftIO $ insertEvent p e
  where
    insertEvent pool (DiscoveredProjectEvent event) = do
      let eventName :: Text = "DISCOVERED_PROJECT"
      withResource pool $ \conn -> withTransaction conn $ do
        _ <- execute
          conn
          [sql|
            INSERT INTO store.store (name, version, payload)
            VALUES (?, ?, ?)
          |] (eventName, version event, payload event)
        return NoContent


receivedStandupUpdateEvent
  :: ReceivedStandupUpdateEvent -> App NoContent
receivedStandupUpdateEvent e = do
  (AppContext p _) <- ask
  liftIO $ insertEvent p e
  where
    insertEvent pool (ReceivedStandupUpdateEvent event) = do
      let eventName :: Text = "RECEIVED_STANDUP_UPDATE"
      withResource pool $ \conn -> withTransaction conn $ do
        _ <- execute
          conn
          [sql|
            INSERT INTO store.store (name, version, payload)
            VALUES (?, ?, ?)
          |] (eventName, version event, payload event)
        return NoContent

server :: ServerT (API auths) App
server (Authenticated _) = discoveredEmployeeEvent
    :<|> discoveredProjectEvent
    :<|> receivedStandupUpdateEvent
-- FIXME: There have to be a better way
server _ = (\_ -> throwM err403) :<|> (\_ -> throwM err403) :<|> (\_ -> throwM err403)
