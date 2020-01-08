module Session.Types where

import           Data.Aeson
import           RIO
import           Servant.Auth.Server as SAS

newtype AccessToken = AccessToken String deriving (Show, Generic, ToJSON, FromJSON)
newtype RefreshToken = RefreshToken String deriving (Show, Generic, ToJSON, FromJSON)

data Session = Session
  { accessToken  :: AccessToken
  , refreshToken :: RefreshToken
  } deriving (Show, Generic, ToJSON, FromJSON, ToJWT, FromJWT)
