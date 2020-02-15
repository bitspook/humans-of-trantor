{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EventInjector
  ( server
  , API
  )
where

import           Data.Pool                        (Pool, withResource)
import           Database.PostgreSQL.Simple       (Connection, execute,
                                                   withTransaction)
import           Database.PostgreSQL.Simple.SqlQQ
import           EventInjector.Types
import           Iam.Session.Types                (AccessToken)
import           RIO
import           Servant                          as S
import           Servant.Auth.Server

discoveredEmployeeEventH
  :: Pool Connection -> DiscoveredEmployeeEvent -> S.Handler NoContent
discoveredEmployeeEventH p e = liftIO $ insertEvent p e
 where
  insertEvent pool (DiscoveredEmployeeEvent event) = do
    let eventName :: Text = "DISCOVERED_EMPLOYEE"
    withResource pool $ \conn -> withTransaction conn $ do
      _ <- execute
        conn
        [sql|
          INSERT INTO store.store (name, version, payload)
          VALUES (?, ?, ?)
          |]
        (eventName, version event, payload event)
      return NoContent

discoveredProjectEventH
  :: Pool Connection -> DiscoveredProjectEvent -> S.Handler NoContent
discoveredProjectEventH p e = liftIO $ insertEvent p e
 where
  insertEvent pool (DiscoveredProjectEvent event) = do
    let eventName :: Text = "DISCOVERED_PROJECT"
    withResource pool $ \conn -> withTransaction conn $ do
      _ <- execute
        conn
        [sql|
          INSERT INTO store.store (name, version, payload)
          VALUES (?, ?, ?)
          |]
        (eventName, version event, payload event)
      return NoContent


receivedStandupUpdateEventH
  :: Pool Connection -> ReceivedStandupUpdateEvent -> S.Handler NoContent
receivedStandupUpdateEventH p e = liftIO $ insertEvent p e
 where
  insertEvent pool (ReceivedStandupUpdateEvent event) = do
    let eventName :: Text = "DISCOVERED_PROJECT"
    withResource pool $ \conn -> withTransaction conn $ do
      _ <- execute
        conn
        [sql|
          INSERT INTO store.store (name, version, payload)
          VALUES (?, ?, ?)
          |]
        (eventName, version event, payload event)
      return NoContent

secureServer :: Pool Connection -> AuthResult AccessToken -> Server SecureAPI
secureServer pool (Authenticated _) =
  discoveredEmployeeEventH pool
    :<|> discoveredProjectEventH pool
    :<|> receivedStandupUpdateEventH pool
secureServer _ _ = throwAll err403

server :: CookieSettings -> JWTSettings -> Pool Connection -> Server (API auths)
server _ _ = secureServer
