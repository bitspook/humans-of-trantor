{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EventInjector
  ( server
  , API
  )
where

import           Data.Pool                          (withResource)
import           Data.UUID
import           Database.PostgreSQL.Simple         (Only (..), query,
                                                     withTransaction)
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.ToField (ToField (..))
import           EventInjector.Types
import           RIO
import           Servant                            as S
import           Servant.Auth.Server
import           Types

insertEvent :: ToField p => Text -> Text -> p -> App UUID
insertEvent eventName version payload = do
  (AppContext pool _) <- ask
  liftIO $ withResource pool $ \conn -> withTransaction conn $ do
    rows :: [Only UUID] <- query
      conn
      [sql|INSERT INTO store.store (name, version, payload) VALUES (?, ?, ?) RETURNING id|]
      (eventName, version, payload)
    case rows of
      [Only x] -> return x
      _        -> throwM err400 { errBody = "Failed to add standup" }

discoveredEmployeeEvent :: DiscoveredEmployee -> App NoContent
discoveredEmployeeEvent e = do
  _ <- insertEvent "DISCOVERED_EMPLOYEE" "v1" e
  return NoContent

discoveredProjectEvent :: DiscoveredProject -> App NoContent
discoveredProjectEvent e = do
  _ <- insertEvent "DISCOVERED_PROJECT" "v1" e
  return NoContent

receivedStandupUpdateEvent :: ReceivedStandupUpdate -> App NoContent
receivedStandupUpdateEvent e = do
  _ <- insertEvent "RECEIVED_STANDUP_UPDATE" "v1" e
  return NoContent

receivedStandupUpdateEventV2 :: ReceivedStandupUpdateV2 -> App NoContent
receivedStandupUpdateEventV2 e = do
  _ <- insertEvent "RECEIVED_STANDUP_UPDATE" "v2" e
  return NoContent

server :: ServerT (API auths) App
server (Authenticated _) =
  discoveredEmployeeEvent
    :<|> discoveredProjectEvent
    :<|> receivedStandupUpdateEvent
    :<|> receivedStandupUpdateEventV2
-- FIXME: There have to be a better way
server _ =
  (\_ -> throwM err403)
    :<|> (\_ -> throwM err403)
    :<|> (\_ -> throwM err403)
    :<|> (\_ -> throwM err403)
