module Identity
  ( API
  , server
  )
where

import           Data.Pool
import           Data.Text
import           Data.Text.Encoding         (decodeUtf8)
import           Database.PostgreSQL.Simple
import           Identity.Types
import           RIO                        hiding (Identity)
import           Servant                    as S
import           Servant.Auth.Server
import           Types

registerIdentity :: Pool Connection -> NewIdentityPayload -> S.Handler Identity
registerIdentity pool (NewIdentityPayload (Email email') password') = do
  rows <- liftIO . withResource pool $ \conn -> query
    conn
    "INSERT INTO iam.identity (email, password) VALUES (?, crypt(?, gen_salt('bf', 10))) RETURNING id, email"
    (toLower email', password')
  case rows of
    (x : _) -> return (Identity (fst x) (Email $ decodeUtf8 . snd $ x))
    []      -> throwError err400

insecureServer :: JWTSettings -> Pool Connection -> Server InsecureAPI
insecureServer _ = registerIdentity

server :: CookieSettings -> JWTSettings -> Pool Connection -> Server API
server _ = insecureServer
