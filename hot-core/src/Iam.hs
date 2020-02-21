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

type API auth = Identity.API :<|> Session.API auth

server :: JWTSettings -> ServerT (API auth) App
server jwts = Identity.server :<|> Session.server jwts
