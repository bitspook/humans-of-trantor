{-# LANGUAGE UnicodeSyntax     #-}

module Lib
  ( runApp
  )
where
import           Network.Wai.Handler.Warp       ( run )
import           RIO
import           Servant
import           Servant.Auth.Server           as SAS
import           Data.Either.Combinators        ( fromRight' )
import           Crypto.JOSE                   as Jose
import           Data.Pool
import           Database.PostgreSQL.Simple (Connection)

import           CryptoUtil                     ( readPemRsaKey )
import           Db                             ( initConnectionPool
                                                , migrate
                                                )

import qualified Session                        ( API
                                                , server
                                                )
import qualified Identity                       ( API
                                                , server
                                                )

type API auths = Identity.API :<|> Session.API auths

server :: CookieSettings -> JWTSettings -> Pool Connection -> Server (API auths)
server c j p = Identity.server c j p :<|> Session.server c j p

runApp :: IO ()
runApp = do
  pool       <- initConnectionPool "postgresql://hot:hot@localhost/iam"
  _          <- liftIO . withResource pool $ migrate "migrations"
  privateKey <- fromRSA . fromRight' <$> readPemRsaKey "./jwt-keys.pem"
  let jwtCfg = JWTSettings privateKey
                           (Just Jose.RS256)
                           (Jose.JWKSet [privateKey])
                           (const Matches)
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy (API '[JWT])
  run 7000 $ serveWithContext api cfg (server defaultCookieSettings jwtCfg pool)
