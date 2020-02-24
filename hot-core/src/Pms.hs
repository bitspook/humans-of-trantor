{-# LANGUAGE TypeOperators #-}

module Pms
  ( API,
    api
  )
where

import qualified Pms.Employee as Employee (API, api)
import qualified Pms.Standup  as Standup (API, api)
import           Servant
import           Types

type API auths = Employee.API auths :<|> Standup.API auths

api :: ServerT (API auths) App
api = Employee.api :<|> Standup.api
