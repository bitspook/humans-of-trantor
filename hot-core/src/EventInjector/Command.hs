{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EventInjector.Command where

import           Data.Pool                          (withResource)
import           Data.UUID
import           Database.PostgreSQL.Simple         (Only (..), query,
                                                     withTransaction)
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.ToField (ToField (..))
import           EventInjector
import           RIO
import           Servant                            as S
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

receivedStandupUpdateEventV2 :: ReceivedStandupUpdateV2 -> App UUID
receivedStandupUpdateEventV2 = insertEvent "RECEIVED_STANDUP_UPDATE" "v2"

deleteStandupUpdateEvent :: DeleteStandupUpdate -> App UUID
deleteStandupUpdateEvent = insertEvent "DELETE_STANDUP_UPDATE" "v1"
