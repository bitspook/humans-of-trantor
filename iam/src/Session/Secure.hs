module Session.Secure
  ( API
  , server
  )
where

import           Data.Aeson
import           RIO
import           Session.Types
import           Servant                       as S
import           Servant.Auth.Server
import           Types
import           Data.Text.Encoding             ( decodeUtf8 )
import           RIO.ByteString.Lazy            ( toStrict )

newtype SessionOpInput = SessionOpInput
  { refreshToken :: RefreshToken } deriving (Show, Generic, FromJSON)

refreshSession
  :: JWTSettings -> AccessToken -> SessionOpInput -> S.Handler Session
refreshSession jwts (AccessToken aId aEmail) (SessionOpInput rt) = do
  token <- liftIO $ makeJWT (AccessToken aId aEmail) jwts Nothing
  case token of
    Left e -> throwError err500
    Right t ->
      return $ Session (decodeUtf8 . toStrict $ t) rt

server :: JWTSettings -> AuthResult AccessToken -> Server API
server jwts (Authenticated a) = refreshSession jwts a
server _    _                 = throwAll err401

type API
  = "session" :> "refresh" :> ReqBody '[JSON] SessionOpInput :> Post '[JSON] Session
