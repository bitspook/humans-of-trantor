{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Pms.Standup.Api where

import           Iam.Session.Types   (AccessToken)
import           Pms.Employee.Types  (Ecode)
import           Pms.Standup
import           Pms.Standup.Query
import           RIO                 hiding (Identity)
import           Servant
import           Servant.Auth.Server
import           Types               (App)

type API auths
  = Auth auths AccessToken :> (
    "standup" :> QueryParam "ecode" Ecode
      :> Get '[JSON] [Standup]
    )

api :: ServerT (API auth) App
api (Authenticated _) = getStandups
api _                 = \_ -> throwM err401
