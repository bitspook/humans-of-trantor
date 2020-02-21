{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Iam.Identity
  ( API
  , server
  )
where

import           Data.Pool                         (withResource)
import           Data.Text                         (toLower)
import           Data.Text.Encoding                (decodeUtf8)
import           Data.UUID                         (UUID)
import           Database.PostgreSQL.Simple        (SqlError (..), query)
import           Database.PostgreSQL.Simple.Errors (ConstraintViolation (UniqueViolation),
                                                    constraintViolation)
import           Database.PostgreSQL.Simple.SqlQQ  (sql)
import           Iam.Identity.Types
import           RIO                               hiding (Identity)
import           Servant.Server
import           Types                             (App, AppContext (..),
                                                    Email (..))

registerIdentity :: NewIdentityPayload -> App Identity
registerIdentity (NewIdentityPayload (Email email') password') = do
  (AppContext pool _)                          <- ask
  rows :: Either SqlError [(UUID, ByteString)] <-
    liftIO $ try $ withResource pool $ \conn -> query
      conn [sql|
        INSERT INTO iam.identity
        (email, password)
        VALUES (?, crypt(?, gen_salt('bf', 10)))
        RETURNING id, email
        |]
      (toLower email', password')
  case rows of
    Right (x : _) -> return (Identity (fst x) (Email $ decodeUtf8 . snd $ x))
    Right _       -> throwM err400
    Left  e       -> case constraintViolation e of
      Just (UniqueViolation _) -> throwM err409
      _                        -> throwM err500

server :: ServerT API App
server = registerIdentity
