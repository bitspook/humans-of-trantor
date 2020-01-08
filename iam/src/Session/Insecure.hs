module Session.Insecure
  ( API
  , server
  )
where

import           Data.Aeson
import           RIO
import           Servant
import           Servant.Auth.Server
import           Session.Types

data NewSessionInput = NewSessionInput
  { email    :: String
  , password :: String
  } deriving (Show, Generic, FromJSON)

type API = "session" :> ReqBody '[JSON] NewSessionInput :> Post '[JSON] Session

server :: CookieSettings -> JWTSettings -> Server API
server _ _ = return . createSession_
 where
  createSession_ :: NewSessionInput -> Session
  createSession_ i = Session (AccessToken "lol") (RefreshToken "rofl")
