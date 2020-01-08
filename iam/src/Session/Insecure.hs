{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Session.Insecure where

import           Data.Aeson
import           RIO
import qualified RIO.ByteString      as BS
import           Servant
import           Servant.Auth        as SA
import           Servant.Auth.Server as SAS
import           Session.Types

data NewSessionInput = NewSessionInput
  { email    :: String
  , password :: String
  } deriving (Show, Generic, FromJSON)

createSession :: CookieSettings -> JWTSettings -> Server InsecureEndpoints
createSession _ _ = \_ -> return $ Session (AccessToken "lol") (RefreshToken "rofl")

type InsecureEndpoints = "session" :> ReqBody '[JSON] NewSessionInput :> Post '[JSON] Session
