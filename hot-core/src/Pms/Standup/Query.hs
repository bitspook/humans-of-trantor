{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Pms.Standup.Query where

import           Data.Pool                        (withResource)
import           Database.PostgreSQL.Simple       (Only (Only), query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Pms.Employee.Types               (Ecode)
import           Pms.Standup                      (Standup)
import           RIO                              hiding (Identity)
import           Types                            (App, AppContext (..))

getStandups :: Maybe Ecode -> App [Standup]
getStandups ecode = do
  (AppContext pool _) <- ask
  let
    baseQuery = [sql|
      WITH standups AS (
        SELECT DISTINCT ON (id)
          CASE WHEN payload->>'source' IS NULL THEN id ELSE (payload->>'source')::UUID END AS id,
          payload->>'ecode' AS ecode,
  	      payload->>'project' AS project,
          payload->>'standup' AS standup,
          payload->>'date' AS date,
          (payload->>'isDelivered')::boolean AS isDelivered
        FROM store.store
        WHERE name = 'RECEIVED_STANDUP_UPDATE'
          AND version = 'v2'
	      ORDER BY id, created_at DESC
      ) SELECT * from standups WHERE 1 = 1
    |]
    ecodeCond = [sql|AND ecode=?|]
  -- FIXME: WHAT THE F
    queryStandups conn = case ecode of
      Nothing ->
        query conn (mconcat [baseQuery]) ()
      Just ecode' ->
        query conn (mconcat [baseQuery, ecodeCond]) (Only ecode')
  liftIO $ withResource pool queryStandups
