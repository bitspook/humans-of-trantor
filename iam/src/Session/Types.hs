module Session.Types
  ( AccessToken (AccessToken)
  , RefreshToken (RefreshToken)
  , Session (Session)
  )
where

import           Data.Aeson
import           RIO
import           Servant.Auth.Server
import           Types

data AccessToken = AccessToken
  { id    :: UUID
  , email :: Email
  } deriving (Show, Generic, ToJSON, FromJSON, ToJWT, FromJWT)

newtype RefreshToken = RefreshToken String deriving (Show, Generic, ToJSON, FromJSON)

data Session = Session
  { accessToken  :: Text
  , refreshToken :: RefreshToken
  } deriving (Show, Generic, ToJSON, FromJSON, ToJWT, FromJWT)
