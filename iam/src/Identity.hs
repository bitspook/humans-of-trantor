module Identity where

import           Data.Aeson
import           Data.Pool
import           Database.PostgreSQL.Simple
import           RIO                     hiding ( Identity )
import           Types
import           Servant as S
import           Servant.Auth.Server

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

registerIdentity :: Pool Connection -> NewIdentityPayload ->  S.Handler Identity
registerIdentity pool (NewIdentityPayload email' password') = do
  res <- liftIO . withResource pool $ \conn -> query conn "INSERT INTO iam.identity (email, password) VALUES (?, crypt(?, gen_salt('bf', 10))) RETURNING id, email, password" (email', password')
  case res of
    (x:_) -> return x
    [] -> throwError err400

insecureServer :: JWTSettings -> Pool Connection -> Server InsecureAPI
insecureServer _ = registerIdentity

server :: CookieSettings -> JWTSettings -> Pool Connection -> Server API
server _ = insecureServer
