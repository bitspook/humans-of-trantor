{-# LANGUAGE TypeOperators #-}

module Iam
  ( API,
    server
  )
where

import qualified Iam.Identity        as Identity (API, server)
import qualified Iam.Session         as Session (API, server)
import           Servant
import           Servant.Auth.Server
import           Types

type API = Identity.API :<|> Session.API

server :: JWTSettings -> ServerT API App
server jwts = Identity.server :<|> Session.server jwts
