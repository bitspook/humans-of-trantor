module Session
  ( API
  , server
  )
where


import           Data.Pool
import           Data.Text.Encoding         (decodeUtf8)
import           Database.PostgreSQL.Simple
import           RIO
import           RIO.ByteString.Lazy        (toStrict)
import           Servant                    as S
import           Servant.Auth.Server
import           Session.Types
import           Types

refreshSession
  :: JWTSettings
  -> Pool Connection
  -> AccessToken
  -> SessionOpInput
  -> S.Handler Session
refreshSession jwts conns (AccessToken aId) (SessionOpInput rt) = do
  v <- liftIO . withResource conns $ \conn -> execute conn "SELECT 1" ()
  traceShowIO v
  token <- liftIO $ makeJWT (AccessToken aId) jwts Nothing
  case token of
    Left  e -> throwError err500 { errBody = fromString . show $ e }
    Right t -> return $ Session (decodeUtf8 . toStrict $ t) rt

secureServer
  :: JWTSettings
  -> Pool Connection
  -> AuthResult AccessToken
  -> Server SecureAPI
secureServer jwts conns (Authenticated a) = refreshSession jwts conns a
secureServer _    _     _                 = throwAll err401

createSession
  :: JWTSettings -> Pool Connection -> NewSessionInput -> S.Handler Session
createSession jwts conns input = do
  _     <- liftIO . withResource conns $ \conn -> execute conn "SELECT 1" ()
  token <- liftIO $ makeJWT (AccessToken (UUID "1")) jwts Nothing
  case token of
    Left  e -> throwError err500 { errBody = fromString . show $ e }
    Right t -> return $ Session (decodeUtf8 $ toStrict t) (RefreshToken "lol")

insecureServer :: JWTSettings -> Pool Connection -> Server InsecureAPI
insecureServer = createSession

server :: CookieSettings -> JWTSettings -> Pool Connection -> Server (API auths)
server _ jwts conns = secureServer jwts conns :<|> insecureServer jwts conns
