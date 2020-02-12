{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Iam.Identity.Types where

import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.UUID                  (UUID)
import           Database.PostgreSQL.Simple (FromRow)
import           RIO                        hiding (Identity)
import           Servant
import           Types                      (Email)

data Identity = Identity
  { id    :: UUID,
    email :: Email
  } deriving(Show, Generic, ToJSON, FromRow)

data NewIdentityPayload = NewIdentityPayload
  { email    :: Email
  , password :: Text
  } deriving(Show, Generic, FromJSON)

type InsecureAPI
  = "identity" :> ReqBody '[JSON] NewIdentityPayload :> Post '[JSON] Identity

type API = InsecureAPI
