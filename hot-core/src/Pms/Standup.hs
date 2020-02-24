{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Pms.Standup
  ( API
  , api
  )
where

import           Data.Pool                        (withResource)
import           Database.PostgreSQL.Simple       (Only (Only), query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)

import           Pms.Employee.Types               (Ecode)
import           Pms.Standup.Types                (API, Standup)
import           RIO                              hiding (Identity)
import           Servant                          as S
import           Servant.Auth.Server
import           Types                            (App, AppContext (..))

getStandups :: Maybe Ecode -> Maybe Integer -> Maybe Integer -> App [Standup]
getStandups ecode month year = do
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
    monthCond =
      [sql|AND date_part('month', (payload->>'date')::Timestamp) = ?|]
    yearCond = [sql|AND date_part('year', (payload->>'date')::Timestamp) = ?|]
  -- FIXME: WHAT THE F
    queryStandups conn = case (ecode, month, year) of
      (Just ecode', Just month', Just year') -> query
        conn
        (mconcat [baseQuery, ecodeCond, monthCond, yearCond])
        (ecode', month', year')
      (Nothing, Just month', Just year') ->
        query conn (mconcat [baseQuery, monthCond, yearCond]) (month', year')
      (Just ecode', Nothing, Just year') ->
        query conn (mconcat [baseQuery, ecodeCond, yearCond]) (ecode', year')
      (Just ecode', Just month', Nothing) ->
        query conn (mconcat [baseQuery, ecodeCond, monthCond]) (ecode', month')
      (Nothing, Nothing, Just year') ->
        query conn (mconcat [baseQuery, yearCond]) (Only year')
      (Just ecode', Nothing, Nothing) ->
        query conn (mconcat [baseQuery, ecodeCond]) (Only ecode')
      (Nothing, Just month', Nothing) ->
        query conn (mconcat [baseQuery, monthCond]) (Only month')
      (Nothing, Nothing, Nothing) -> query conn baseQuery ()
  liftIO $ withResource pool queryStandups

api :: ServerT (API auth) App
api (Authenticated _) = getStandups
api _                 = \_ _ _ -> throwM err401
