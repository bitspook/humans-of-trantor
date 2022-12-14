{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}

module Iam.Session.Types where

import           Data.Aeson
import           Data.UUID
import           RIO
import           Servant
import           Servant.Auth.Server
import           Types

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

type API
  = "session" :> ReqBody '[JSON] NewSessionInput :> Post '[JSON] Session
    :<|> "session" :> ReqBody '[JSON] SessionOpInput :> Put '[JSON] Session
    :<|> "session" :> ReqBody '[JSON] SessionOpInput :> Delete '[JSON] NoContent
