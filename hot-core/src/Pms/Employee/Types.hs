{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Pms.Employee.Types where

import           Data.Aeson                           (FromJSON, ToJSON)
import           Database.PostgreSQL.Simple.FromField (FromField (..))
import           Database.PostgreSQL.Simple.FromRow   (FromRow (..))
import           Database.PostgreSQL.Simple.ToField   (Action (..),
                                                       ToField (..))
import           Iam.Session.Types                    (AccessToken)
import           RIO
import           Servant
import           Servant.Auth.Server
import           Types                                (Email, fromTextField)

-- ProjectName
newtype ProjectName = ProjectName Text deriving (Show, Generic, ToJSON, FromJSON)

instance FromField ProjectName where
  fromField = fromTextField ProjectName

instance ToField ProjectName where
  toField (ProjectName a) = Escape $ encodeUtf8 a

instance FromHttpApiData ProjectName where
  parseQueryParam p = Right (ProjectName p)
---

-- ecode
newtype Ecode = Ecode Text deriving (Show, Generic, ToJSON, FromJSON)

instance FromField Ecode where
  fromField = fromTextField Ecode

instance ToField Ecode where
  toField (Ecode a) = Escape $ encodeUtf8 a

instance FromHttpApiData Ecode where
  parseQueryParam p = Right (Ecode p)
--

-- EmployeeName
newtype EmployeeName = EmployeeName Text deriving (Show, Generic, ToJSON, FromJSON)

instance FromField EmployeeName where
  fromField = fromTextField EmployeeName

instance ToField EmployeeName where
  toField (EmployeeName a) = Escape $ encodeUtf8 a
---

-- Designation
newtype Designation = Designation Text deriving (Show, Generic, ToJSON, FromJSON)

instance FromField Designation where
  fromField = fromTextField Designation

instance ToField Designation where
  toField (Designation a) = Escape $ encodeUtf8 a
---

data Employee = Employee
  { ecode       :: Ecode
  , name        :: EmployeeName
  , email       :: Email
  , project     :: Maybe ProjectName
  , designation :: Designation
  } deriving (Show, Generic, ToJSON, FromJSON, FromRow)

type SecureAPI
  = "employees" :> QueryParam "project" ProjectName :> Get '[JSON] [Employee]

type InsecureAPI = "placeholder" :> Post '[JSON] NoContent

type API auths = (Auth auths AccessToken :> SecureAPI) :<|> InsecureAPI
