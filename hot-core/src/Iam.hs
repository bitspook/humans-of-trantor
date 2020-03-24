{-# LANGUAGE TypeOperators #-}

module Iam
  ( API,
    api
  )
where

import qualified Iam.Identity        as Identity (API, api)
import qualified Iam.Session         as Session (API, api)
import           Servant
import           Servant.Auth.Server
import           Types

type API = Identity.API :<|> Session.API

api :: JWTSettings -> ServerT API App
api jwts = Identity.api :<|> Session.api jwts
