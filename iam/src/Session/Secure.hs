module Session.Secure where

import Data.Aeson
import RIO
import Session.Types
import Servant
import Servant.Auth.Server

data SessionOpInput = SessionOpInput
  { refreshToken :: String } deriving (Show, Generic, FromJSON)

type SecureEndpoints = "session" :> ReqBody '[JSON] SessionOpInput :> Put '[JSON] Session

refreshSession :: AuthResult Session -> Server SecureEndpoints
refreshSession (Authenticated s) = \_ -> return s
refreshSession _                 = throwAll err401
