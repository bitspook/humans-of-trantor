{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pms.Standup
  ( API
  , server
  )
where


import           Data.Pool                        (Pool, withResource)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Iam.Session.Types                (AccessToken)
import           Pms.Employee.Types               (Ecode)
import           Pms.Standup.Types                (API, InsecureAPI, SecureAPI,
                                                   Standup)
import           RIO                              hiding (Identity)
import           Servant                          as S
import           Servant.Auth.Server

getStandups :: Pool Connection -> Maybe Ecode -> Maybe Integer -> Maybe Integer -> IO [Standup]
getStandups pool ecode month year = do
  let baseQuery' =
        [sql|SELECT DISTINCT ON (payload->>'date', payload->>'type')
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
      monthCond = [sql|AND date_part('month', (payload->>'date')::Timestamp) = ?|]
      yearCond = [sql|AND date_part('year', (payload->>'date')::Timestamp) = ?|]
        -- FIXME: WHAT THE F
      queryStandups conn = case (ecode, month, year) of
        (Just ecode', Just month', Just year') -> query conn (mconcat [baseQuery', ecodeCond, monthCond, yearCond]) (ecode', month', year')
        (Nothing, Just month', Just year') -> query conn (mconcat [baseQuery', monthCond, yearCond]) (month', year')
        (Just ecode', Nothing, Just year') -> query conn (mconcat [baseQuery', ecodeCond, yearCond]) (ecode', year')
        (Just ecode', Just month', Nothing) -> query conn (mconcat [baseQuery', ecodeCond, monthCond]) (ecode', month')
        (Nothing, Nothing, Just year') -> query conn (mconcat [baseQuery', yearCond]) (Only year')
        (Just ecode', Nothing, Nothing) -> query conn (mconcat [baseQuery', ecodeCond]) (Only ecode')
        (Nothing, Just month', Nothing) -> query conn (mconcat [baseQuery', monthCond]) (Only month')
        (Nothing, Nothing, Nothing) -> query conn baseQuery' ()
  withResource pool queryStandups

getStandupsH :: Pool Connection -> Maybe Ecode -> Maybe Integer -> Maybe Integer -> S.Handler [Standup]
getStandupsH pool e m y = liftIO $ getStandups pool e m y

insecureServer :: JWTSettings -> Pool Connection -> Server InsecureAPI
insecureServer _ _ = throwError err500

secureServer :: Pool Connection -> AuthResult AccessToken -> Server SecureAPI
secureServer pool (Authenticated _) = getStandupsH pool
secureServer _    _                 = \_ _ _ -> throwError err401

server :: CookieSettings -> JWTSettings -> Pool Connection -> Server (API auths)
server _ jwts pool = secureServer pool :<|> insecureServer jwts pool
