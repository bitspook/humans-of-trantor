{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Iam
  ( app
  )
where
import           Crypto.JOSE                as Jose
import           Crypto.JOSE.JWK            (JWK)
import           Data.Pool
import           Database.PostgreSQL.Simple (Connection)
import qualified Iam.Identity               as Identity (API, server)
import qualified Iam.Session                as Session (API, server)
import           RIO
import           Servant
import           Servant.Auth.Server        as SAS

type API auths = Identity.API :<|> Session.API auths

server :: CookieSettings -> JWTSettings -> Pool Connection -> Server (API auths)
server c j p = Identity.server c j p :<|> Session.server c j p

app :: JWK -> Pool Connection -> Application
app privateKey pool = do
  let jwtCfg = JWTSettings privateKey
                           (Just Jose.RS256)
                           (Jose.JWKSet [privateKey])
                           (const Matches)
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy (API '[JWT])
  serveWithContext api cfg (server defaultCookieSettings jwtCfg pool)
