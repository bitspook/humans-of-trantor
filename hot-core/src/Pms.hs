{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Pms
  ( API,
    server
  )
where
import           Data.Pool
import           Database.PostgreSQL.Simple (Connection)
import qualified Pms.Employee               as Employee (API, server)
import qualified Pms.Standup                as Standup (API, server)
import           Servant
import           Servant.Auth.Server        as SAS

type API auths = Employee.API auths :<|> Standup.API auths

server :: CookieSettings -> JWTSettings -> Pool Connection -> Server (API auths)
server c j p = Employee.server c j p :<|> Standup.server c j p
