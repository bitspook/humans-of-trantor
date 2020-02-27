{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module EventInjector.Api where

import           Data.UUID
import           EventInjector
import           EventInjector.Command
import           Iam.Session.Types     (AccessToken)
import           RIO
import           Servant
import           Servant.Auth.Server
import           Types

type API auths =  Auth auths AccessToken :>
  "event" :> (
  "DISCOVERED_EMPLOYEE" :> "v1" :> ReqBody '[JSON] DiscoveredEmployee :> Post '[JSON] NoContent
    :<|> "DISCOVERED_PROJECT" :> "v1" :> ReqBody '[JSON] DiscoveredProject :> Post '[JSON] NoContent
    :<|> "RECEIVED_STANDUP_UPDATE" :> "v2" :> ReqBody '[JSON] ReceivedStandupUpdateV2 :> Post '[JSON] UUID
  )

api :: ServerT (API auths) App
api (Authenticated _) =
  discoveredEmployeeEvent
    :<|> discoveredProjectEvent
    :<|> receivedStandupUpdateEventV2
-- FIXME: There have to be a better way
api _ =
  (\_ -> throwM err403)
    :<|> (\_ -> throwM err403)
    :<|> (\_ -> throwM err403)
