{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Lib
  ( runApp  )
where
import           Network.Wai.Handler.Warp (run)
import           RIO
import           Servant
import           Servant.Auth.Server      as SAS
import qualified Session                  as S
import           System.IO

runApp = do
  myKey <- SAS.generateKey
  let jwtCfg = defaultJWTSettings myKey
      cfg    = defaultCookieSettings :. jwtCfg :. EmptyContext
      api    = Proxy :: Proxy (S.Api '[JWT])
  run 7000 $ serveWithContext api cfg (S.server defaultCookieSettings jwtCfg)
