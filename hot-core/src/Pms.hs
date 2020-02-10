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
import           Servant
import           Servant.Auth.Server        as SAS

type API auths = Employee.API auths

server :: CookieSettings -> JWTSettings -> Pool Connection -> Server (API auths)
server = Employee.server
