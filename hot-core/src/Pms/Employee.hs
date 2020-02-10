{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pms.Employee
  ( API
  , server
  )
where


import           Data.Pool                  (Pool)
import           Database.PostgreSQL.Simple (Connection)
import           Iam.Session.Types          (AccessToken)
import           Pms.Employee.Types         (API, Employee, InsecureAPI,
                                             SecureAPI)
import           RIO                        hiding (Identity)

import           Servant                    as S
import           Servant.Auth.Server

getEmployeesH :: Pool Connection -> S.Handler [Employee]
getEmployeesH _ = return []

insecureServer :: JWTSettings -> Pool Connection -> Server InsecureAPI
insecureServer _ _ = throwError err500

secureServer :: Pool Connection -> AuthResult AccessToken -> Server SecureAPI
secureServer pool (Authenticated _) = getEmployeesH pool
secureServer _    _                 = throwError err401

server :: CookieSettings -> JWTSettings -> Pool Connection -> Server (API auths)
server _ jwts pool = secureServer pool :<|> insecureServer jwts pool
