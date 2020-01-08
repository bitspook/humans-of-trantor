{-# LANGUAGE UnicodeSyntax     #-}

module Lib
  ( runApp  )
where
import           Network.Wai.Handler.Warp (run)
import           RIO
import           Servant
import           Servant.Auth.Server      as SAS
import           Session (API, server)

runApp = do
  myKey <- SAS.generateKey
  let jwtCfg = defaultJWTSettings myKey
      cfg    = defaultCookieSettings :. jwtCfg :. EmptyContext
      api    = Proxy :: Proxy (API '[JWT])
  run 7000 $ serveWithContext api cfg (server defaultCookieSettings jwtCfg)
