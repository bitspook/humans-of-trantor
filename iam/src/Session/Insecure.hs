module Session.Insecure
  ( API
  , server
  )
where

import           Data.Aeson
import           RIO
import           Servant                       as S
import           Servant.Auth.Server
import           Types
import           Session.Types
import           Data.Text.Encoding             ( decodeUtf8 )
import           RIO.ByteString.Lazy            ( toStrict )

data NewSessionInput = NewSessionInput
  { email    :: Email
  , password :: Text
  } deriving (Show, Generic, FromJSON)

type API = "session" :> ReqBody '[JSON] NewSessionInput :> Post '[JSON] Session

createSession :: JWTSettings -> NewSessionInput -> S.Handler Session
createSession jwts i = do
  token <- liftIO $ makeJWT (AccessToken (UUID "1") (email i)) jwts Nothing
  case token of
    Left  e -> throwError err500
    Right t -> return $ Session (decodeUtf8 $ toStrict t) (RefreshToken "lol")

server :: JWTSettings -> Server API
server = createSession
