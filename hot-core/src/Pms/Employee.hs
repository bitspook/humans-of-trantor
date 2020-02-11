{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pms.Employee
  ( API
  , server
  )
where


import           Data.Pool                        (Pool, withResource)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Iam.Session.Types                (AccessToken)
import           Pms.Employee.Types               (API, Employee, InsecureAPI,
                                                   SecureAPI)
import           RIO                              hiding (Identity)
import           Servant                          as S
import           Servant.Auth.Server

getEmployees :: Pool Connection -> Maybe Text -> IO [Employee]
getEmployees pool project = do
  let baseQuery' = [sql|
            SELECT DISTINCT on (payload->>'ecode')
              payload->>'ecode' as ecode,
              payload->>'name' as name, payload->>'email' as email,
              payload->>'project' as project,
              payload->>'designation' as designation
            FROM store.store
            WHERE name = 'DISCOVERED_EMPLOYEE'
            |]
      projectCond = [sql|AND  payload->>'project'=?|]
      queryEmployees conn = case project of
        Just p  -> query conn (mappend baseQuery' projectCond) (Only p)
        Nothing -> query conn baseQuery' ()
  withResource pool queryEmployees

getEmployeesH :: Pool Connection -> Maybe Text -> S.Handler [Employee]
getEmployeesH pool project = liftIO $ getEmployees pool project

insecureServer :: JWTSettings -> Pool Connection -> Server InsecureAPI
insecureServer _ _ = throwError err500

secureServer :: Pool Connection -> AuthResult AccessToken -> Server SecureAPI
secureServer pool (Authenticated _) = getEmployeesH pool
secureServer _    _                 = \_ -> throwError err401

server :: CookieSettings -> JWTSettings -> Pool Connection -> Server (API auths)
server _ jwts pool = secureServer pool :<|> insecureServer jwts pool
