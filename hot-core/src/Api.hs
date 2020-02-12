{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api
  ( run
  )
where
import           Crypto.JOSE                          (Alg (RS256),
                                                       JWKSet (JWKSet), fromRSA)
import           Crypto.JOSE.JWK                      (JWK)
import           CryptoUtil                           (readPemRsaKey)
import           Data.Either.Combinators              (fromRight')
import           Data.Pool                            (Pool, withResource)
import           Database.PostgreSQL.Simple           (Connection)
import           Db                                   (initConnectionPool,
                                                       migrate)
import           Dhall                                (auto, input)
import qualified Iam                                  (API, server)
import           Network.Wai                          (Middleware)
import qualified Network.Wai.Handler.Warp             as Warp (run)
import           Network.Wai.Middleware.Cors          (cors, corsRequestHeaders,
                                                       simpleCorsResourcePolicy)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Pms                                  (API, server)
import           RIO.Text                             (encodeUtf8, unpack)
import           Servant
import           Servant.Auth.Server

import           Types

corsMiddleware :: Middleware
corsMiddleware = cors
  (const $ Just
    (simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type", "Authorization"] })
  )

type API auths = "api" :> (Iam.API auths :<|> Pms.API auths)

server :: CookieSettings -> JWTSettings -> Pool Connection -> Server (API auths)
server c j p = Iam.server c j p :<|> Pms.server c j p

app :: JWK -> Pool Connection -> Application
app privateKey pool = do
  let jwtCfg = JWTSettings privateKey
                           (Just RS256)
                           (JWKSet [privateKey])
                           (const Matches)
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy (API '[JWT])
  serveWithContext api cfg (server defaultCookieSettings jwtCfg pool)

run :: IO ()
run = do
  conf       <- input auto "./config.dhall"
  pool       <- initConnectionPool . encodeUtf8 . dbUrl $ conf
  _          <- withResource pool $ migrate (unpack . migrationsDir $ conf)
  privateKey <- fromRSA . fromRight' <$> readPemRsaKey
    (unpack . jwtKeysPath $ conf)
  Warp.run (fromIntegral $ port conf) $ corsMiddleware $ logStdoutDev $ app privateKey pool
