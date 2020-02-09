{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
  ( runApp
  )
where
import           Crypto.JOSE                 as Jose
import           Data.Either.Combinators     (fromRight')
import           Data.Pool
import           Database.PostgreSQL.Simple  (Connection)
import           Network.Wai.Handler.Warp    (run)
import           RIO
import           RIO.Text                    (unpack)
import           Servant
import           Servant.Auth.Server         as SAS

import           CryptoUtil                  (readPemRsaKey)
import           Db                          (initConnectionPool, migrate)
import           Dhall
import qualified Identity                    (API, server)
import           Network.Wai                 (Middleware)
import           Network.Wai.Middleware.Cors (cors, corsRequestHeaders,
                                              simpleCorsResourcePolicy)
import qualified Session                     (API, server)

type API auths = Identity.API :<|> Session.API auths

data Config = Config
  { dbUrl         :: Text,
    jwtKeysPath   :: Text,
    migrationsDir :: Text,
    port          :: Natural
  } deriving (Generic, Show)

instance Interpret Config

server :: CookieSettings -> JWTSettings -> Pool Connection -> Server (API auths)
server c j p = Identity.server c j p :<|> Session.server c j p

corsMiddleware :: Middleware
corsMiddleware = cors ( const $ Just (simpleCorsResourcePolicy  { corsRequestHeaders = ["Content-Type"] }) )

runApp :: IO ()
runApp = do
  conf       <- input auto "./config.dhall"
  pool       <- initConnectionPool . encodeUtf8 . dbUrl $ conf
  _          <- withResource pool $ migrate (unpack . migrationsDir $ conf)
  privateKey <- fromRSA . fromRight' <$> readPemRsaKey (unpack . jwtKeysPath $ conf)
  let jwtCfg = JWTSettings privateKey
                           (Just Jose.RS256)
                           (Jose.JWKSet [privateKey])
                           (const Matches)
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy (API '[JWT])
  run (fromIntegral $ port conf)
    $ corsMiddleware $ serveWithContext api cfg (server defaultCookieSettings jwtCfg pool)
