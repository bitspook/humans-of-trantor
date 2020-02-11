{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Pms.Employee.Types where

import           Data.Aeson                         (FromJSON, ToJSON)
import           Database.PostgreSQL.Simple.FromRow (FromRow)
import           Iam.Session.Types                  (AccessToken)
import           RIO                                (Generic, Maybe, Show, Text)
import           Servant
import           Servant.Auth.Server

data Employee = Employee
  { ecode       :: Text
  , name        :: Text
  , email       :: Text
  , project     :: Maybe Text
  , designation :: Text
  } deriving (Show, Generic, ToJSON, FromJSON, FromRow)

type SecureAPI = "employees" :> QueryParam "project" Text :> Get '[JSON] [Employee]

type InsecureAPI
  = "placeholder" :> Post '[JSON] NoContent

type API auths = (Auth auths AccessToken :> SecureAPI) :<|> InsecureAPI
