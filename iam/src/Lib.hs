{-# LANGUAGE UnicodeSyntax     #-}

module Lib
  ( runApp
  )
where
import           Network.Wai.Handler.Warp       ( run )
import           RIO
import           Servant
import           Servant.Auth.Server           as SAS
import           Session                        ( API
                                                , server
                                                )
import Data.Either.Combinators (fromRight')
import           Crypto.JOSE                   as Jose
import Crypto.PubKey.RSA.Types
import Crypto.PubKey.OpenSsh
import CryptoUtil (readPemRsaKey)

runApp = do
  myKey      <- SAS.generateKey
  privateKey <- fromRSA . fromRight' <$> readPemRsaKey "./jwt-keys.pem"
  let
    jwtCfg = JWTSettings privateKey (Just Jose.RS256) (Jose.JWKSet [privateKey]) (const Matches)
    cfg    = defaultCookieSettings :. jwtCfg :. EmptyContext
    api    = Proxy :: Proxy (API '[JWT])
  run 7000 $ serveWithContext api cfg (server defaultCookieSettings jwtCfg)
