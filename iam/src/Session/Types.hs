module Session.Types where

import           Data.Aeson
import           RIO
import           Servant.Auth.Server
import           Types
import           Servant

newtype AccessToken = AccessToken
  { id    :: UUID
  } deriving (Show, Generic, ToJSON, FromJSON, ToJWT, FromJWT)

newtype RefreshToken = RefreshToken String deriving (Show, Generic, ToJSON, FromJSON)

data Session = Session
  { accessToken  :: Text
  , refreshToken :: RefreshToken
  } deriving (Show, Generic, ToJSON, FromJSON, ToJWT, FromJWT)

newtype SessionOpInput = SessionOpInput
  { refreshToken :: RefreshToken } deriving (Show, Generic, FromJSON)

data NewSessionInput = NewSessionInput
  { email    :: Email
  , password :: Text
  } deriving (Show, Generic, FromJSON)

type SecureAPI
  = "session" :> "refresh" :> ReqBody '[JSON] SessionOpInput :> Post '[JSON] Session
type InsecureAPI
  = "session" :> ReqBody '[JSON] NewSessionInput :> Post '[JSON] Session
type API auths = (Auth auths AccessToken :> SecureAPI) :<|> InsecureAPI
