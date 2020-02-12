{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Pms.Standup.Types where

import           Data.Aeson                           (FromJSON, ToJSON)
import           Database.PostgreSQL.Simple.FromField (FromField (..))
import           Database.PostgreSQL.Simple.FromRow   (FromRow (..))
import           Database.PostgreSQL.Simple.ToField   (Action (..),
                                                       ToField (..))
import           Iam.Session.Types                    (AccessToken)
import           Pms.Employee.Types                   (Ecode, ProjectName)
import           RIO
import           Servant
import           Servant.Auth.Server
import           Types                                (fromTextField)

-- StandupBody
newtype StandupBody = StandupBody Text deriving (Show, Generic, ToJSON, FromJSON)

instance FromField StandupBody where
  fromField = fromTextField StandupBody

instance ToField StandupBody where
  toField (StandupBody a) = Escape $ encodeUtf8 a
---

-- StandupType
newtype StandupType = StandupType Text deriving (Show, Generic, ToJSON, FromJSON)

instance FromField StandupType where
  fromField = fromTextField StandupType

instance ToField StandupType where
  toField (StandupType a) = Escape $ encodeUtf8 a
---

data Standup = Standup
  { ecode       :: Ecode
  , project     :: ProjectName
  , standup     :: StandupBody
  , date        :: Text
  , standupType :: StandupType
  } deriving (Show, Generic, ToJSON, FromJSON, FromRow)

type SecureAPI
  = "standup" :> QueryParam "ecode" Ecode :> Get '[JSON] [Standup]

type InsecureAPI = "placeholder" :> Post '[JSON] NoContent

type API auths = (Auth auths AccessToken :> SecureAPI) :<|> InsecureAPI
