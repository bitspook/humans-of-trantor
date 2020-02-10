{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Pms.Employee.Types where

import           Data.Aeson          (FromJSON, ToJSON)
import           Iam.Session.Types   (AccessToken)
import           RIO                 (Generic, Show, Text)
import           Servant
import           Servant.Auth.Server


data Employee = Employee
  { name        :: Text
  , ecode       :: Text
  , email       :: Text
  , project     :: Text
  , designation :: Text
  } deriving (Show, Generic, ToJSON, FromJSON)

type SecureAPI = "employees" :> Get '[JSON] [Employee]

type InsecureAPI
  = "placeholder" :> Post '[JSON] NoContent

type API auths = (Auth auths AccessToken :> SecureAPI) :<|> InsecureAPI
