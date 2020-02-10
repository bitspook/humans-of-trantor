{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Iam
  ( API,
    server
  )
where


import           Data.Pool
import           Database.PostgreSQL.Simple (Connection)
import qualified Iam.Identity               as Identity (API, server)
import qualified Iam.Session                as Session (API, server)
import           Servant
import           Servant.Auth.Server        as SAS

type API auths = Identity.API :<|> Session.API auths

server :: CookieSettings -> JWTSettings -> Pool Connection -> Server (API auths)
server c j p = Identity.server c j p :<|> Session.server c j p
