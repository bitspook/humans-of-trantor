{-# LANGUAGE TypeOperators #-}

module Pms
  ( API,
    server
  )
where

import qualified Pms.Employee as Employee (API, server)
import qualified Pms.Standup  as Standup (API, server)
import           Servant
import           Types

type API auths = Employee.API auths :<|> Standup.API auths

server :: ServerT (API auths) App
server = Employee.server :<|> Standup.server
