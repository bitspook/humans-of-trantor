{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Iam.Session
  ( API
  , server
  )
where


import           Data.Pool
import           Data.Text
import           Data.Text.Encoding         (decodeUtf8)
import           Data.UUID                  (UUID)
import           Database.PostgreSQL.Simple
import           Iam.Identity.Types         (Identity (Identity))
import           Iam.Session.Types
import           RIO                        hiding (Identity)
import           RIO.ByteString.Lazy        (toStrict)
import           Servant                    as S
import           Servant.Auth.Server
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

getSession :: Pool Connection -> RefreshToken -> IO (Maybe (RefreshToken, UUID))
getSession pool (RefreshToken sid) = do
  rows :: [(UUID, UUID)] <- liftIO . withResource pool $ \conn -> query
    conn
    "SELECT id, identity_id from iam.session WHERE id = ? AND revoked_at is NULL"
    (Only sid)
  case rows of
    ((s, iid) : _) -> return $ Just (RefreshToken $ show s, iid)
    []             -> return Nothing

createSession :: Pool Connection -> UUID -> IO (Maybe RefreshToken)
createSession pool aid = do
  rows :: Either SqlError [(Only UUID)] <-
    liftIO $ try $ withResource pool $ \conn -> query
      conn
      "INSERT INTO iam.session (identity_id) VALUES (?) RETURNING id"
      (Only aid)
  case rows of
    Right (Only x : _) -> return $ Just (RefreshToken $ show x)
    Right _            -> return Nothing
    Left  _            -> return Nothing

revokeSession :: Pool Connection -> RefreshToken -> IO ()
revokeSession pool (RefreshToken sid) = do
  _ :: Either SqlError [(Only UUID)] <-
    liftIO $ try $ withResource pool $ \conn -> query
      conn
      "UPDATE iam.session SET revoked_at = NOW() WHERE id = ? and revoked_at IS NULL RETURNING id"
      (Only sid)
  return ()

refreshSession
  :: JWTSettings -> Pool Connection -> SessionOpInput -> S.Handler Session
refreshSession jwts pool (SessionOpInput rt) = do
  session' <- liftIO $ getSession pool rt
  case session' of
    Nothing            -> throwError err403
    Just (rtoken, iid) -> do
      token <- liftIO $ makeJWT (AccessToken iid) jwts Nothing
      case token of
        Left  e -> throwError err500 { errBody = fromString . show $ e }
        Right t -> return $ Session (decodeUtf8 . toStrict $ t) rtoken

createSessionH
  :: JWTSettings -> Pool Connection -> NewSessionInput -> S.Handler Session
createSessionH jwts pool inp = do
  maybeId <- liftIO $ getIdentity pool inp
  case maybeId of
    Nothing               -> throwError err401
    Just (Identity id' _) -> do
      session' <- liftIO $ createSession pool id'
      case session' of
        Nothing      -> throwError err500
        Just session -> do
          token <- liftIO $ makeJWT (AccessToken id') jwts Nothing
          case token of
            Left  e -> throwError err500 { errBody = fromString . show $ e }
            Right t -> return $ Session (decodeUtf8 $ toStrict t) session

revokeSessionH :: Pool Connection -> SessionOpInput -> S.Handler NoContent
revokeSessionH pool (SessionOpInput rs) = do
  liftIO $ revokeSession pool rs
  return NoContent

insecureServer :: JWTSettings -> Pool Connection -> Server InsecureAPI
insecureServer jwts pool =
  createSessionH jwts pool
    :<|> refreshSession jwts pool
    :<|> revokeSessionH pool

secureServer
  :: JWTSettings
  -> Pool Connection
  -> AuthResult AccessToken
  -> Server SecureAPI
secureServer _ _ _ = throwAll err401

server :: CookieSettings -> JWTSettings -> Pool Connection -> Server (API auths)
server _ jwts pool = secureServer jwts pool :<|> insecureServer jwts pool
