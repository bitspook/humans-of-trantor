module Session
  ( API
  , server
  )
where


import           Data.Pool
import           Data.Text
import           Data.Text.Encoding             ( decodeUtf8 )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple
import           Identity.Types                 ( Identity(Identity) )
import           RIO                     hiding ( Identity )
import           RIO.ByteString.Lazy            ( toStrict )
import           Servant                       as S
import           Servant.Auth.Server
import           Session.Types
import           Types

getIdentity :: Pool Connection -> NewSessionInput -> IO (Maybe Identity)
getIdentity pool (NewSessionInput (Email email') pass') = do
  rows <- liftIO . withResource pool $ \conn -> query
    conn
    "SELECT id, email from iam.identity WHERE email = ? and password = crypt(?, password)"
    (toLower email', pass')
  case rows of
    (x : _) -> return $ Just (Identity (fst x) (Email $ decodeUtf8 . snd $ x))
    []      -> return Nothing

refreshSession
  :: JWTSettings
  -> Pool Connection
  -> AccessToken
  -> SessionOpInput
  -> S.Handler Session
refreshSession jwts pool (AccessToken aId) (SessionOpInput rt) = do
  v <- liftIO . withResource pool $ \conn -> execute conn "SELECT 1" ()
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
secureServer jwts pool (Authenticated a) = refreshSession jwts pool a
secureServer _    _    _                 = throwAll err401

createSession :: Pool Connection -> UUID -> IO (Maybe RefreshToken)
createSession pool aid = do
  rows :: Either SqlError [(Only UUID)] <-
    liftIO $ try $ withResource pool $ \conn -> query
      conn
      "INSERT INTO iam.session (identity_id) VALUES (?) RETURNING id"
      (Only aid)
  case rows of
    Right (Only x : _) -> return $ Just (RefreshToken $ show x)
    Right _       -> return Nothing
    Left  _       -> return Nothing

createSessionH
  :: JWTSettings -> Pool Connection -> NewSessionInput -> S.Handler Session
createSessionH jwts pool inp = do
  maybeId <- liftIO $ getIdentity pool inp

  case maybeId of
    Nothing               -> throwError err401
    Just (Identity id' _) -> do
      session' <- liftIO $ createSession pool id'
      case session' of
        Nothing -> throwError err500
        Just session -> do
          token <- liftIO $ makeJWT (AccessToken id') jwts Nothing
          case token of
            Left e -> throwError err500 { errBody = fromString . show $ e }
            Right t ->
              return $ Session (decodeUtf8 $ toStrict t) session

insecureServer :: JWTSettings -> Pool Connection -> Server InsecureAPI
insecureServer = createSessionH

server :: CookieSettings -> JWTSettings -> Pool Connection -> Server (API auths)
server _ jwts pool = secureServer jwts pool :<|> insecureServer jwts pool
