module Identity.Types where

import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           Data.UUID                      ( UUID )
import           Types                          ( Email )
import           Servant
import           RIO                     hiding ( Identity )
import           Database.PostgreSQL.Simple     ( FromRow )

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
