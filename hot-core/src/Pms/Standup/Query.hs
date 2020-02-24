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
    baseQuery = [sql|SELECT DISTINCT ON (payload->>'date', payload->>'type')
            payload->>'ecode' as ecode,
            payload->>'project' as project,
            payload->>'standup' as standup,
            payload->>'date' as date,
            payload->>'type' as standupType
          FROM (
            SELECT payload from store.store
            WHERE name = 'RECEIVED_STANDUP_UPDATE'
            AND version = 'v1'
            ORDER BY created_at DESC
          ) AS store
          WHERE 1 = 1
          |]
    ecodeCond = [sql|AND payload->>'ecode'=?|]
  -- FIXME: WHAT THE F
    queryStandups conn = case ecode of
      Nothing ->
        query conn (mconcat [baseQuery]) ()
      Just ecode' ->
        query conn (mconcat [baseQuery, ecodeCond]) (Only ecode')
  liftIO $ withResource pool queryStandups
