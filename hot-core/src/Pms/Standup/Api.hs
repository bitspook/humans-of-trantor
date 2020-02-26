{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Pms.Standup.Api where

import           Iam.Session.Types   (AccessToken)
import           Pms.Employee.Types  (Ecode)
import           Pms.Standup
import           Pms.Standup.Command
import           Pms.Standup.Query
import           RIO                 (throwM)
import           Servant
import           Servant.Auth.Server
import           Types               (App)

type API auths
  = Auth auths AccessToken :> (
    "standup" :> QueryParam "ecode" Ecode :> Get '[JSON] [Standup]
    :<|> "standup" :> ReqBody '[JSON] StandupData :> Post '[JSON] Standup
    :<|> "standup" :> Capture "sid" StandupId :> ReqBody '[JSON] StandupData :> Put '[JSON] Standup
    )

api :: ServerT (API auth) App
api (Authenticated _) = getStandups :<|> addStandup :<|> replaceStandup
api _                 = (\_ -> throwM err401):<|> (\_ -> throwM err401) :<|> (\_ _ -> throwM err401)