module Session.Secure
  ( API
  , server
  )
where

import           Data.Aeson
import           RIO
import           Session.Types
import           Servant
import           Servant.Auth.Server

data SessionOpInput = SessionOpInput
  { refreshToken :: String } deriving (Show, Generic, FromJSON)

refreshSession :: Session -> SessionOpInput -> Session
refreshSession s _ = s

server :: AuthResult Session -> Server API
server (Authenticated s) = return . refreshSession s
server _                 = throwAll err401

type API = "session" :> ReqBody '[JSON] SessionOpInput :> Put '[JSON] Session
