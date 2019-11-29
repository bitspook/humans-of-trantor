{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Session
  ( Spi,
    server
  ) where
import qualified Crypto.JWT               as Jwt
import           Crypto.PubKey.RSA        (PrivateKey)
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import           Data.X509
import           Data.X509.File
import           Magicbane
import           RIO
import qualified RIO.ByteString           as BS

data NewSessionInput = NewSessionInput
  { email    :: String
  , password :: String
  } deriving (Show, Generic, FromJSON)

data SessionOpInput = SessionOpInput
  { refreshToken :: String } deriving (Show, Generic, FromJSON)

data Session = Session
  { accessToken  :: String
  , refreshToken :: String
  } deriving (Show, Generic, ToJSON)

createSession :: Monad m => NewSessionInput -> m Session
createSession _ = do
  return $ Session {accessToken = "lol", refreshToken = "rofl"}

refreshSession :: Monad m => SessionOpInput -> m Session
refreshSession _ = do
  return $ Session {accessToken = "rofl", refreshToken = "lol"}

revokeSession :: Monad m => SessionOpInput -> m ()
revokeSession _ = do
  return ()

type Spi = "session" :> ReqBody '[JSON] NewSessionInput :> Post '[JSON] Session
  :<|> "session" :> "refresh" :> ReqBody '[JSON] SessionOpInput :> Post '[JSON] Session
  :<|> "session" :> "revoke" :> ReqBody '[JSON] SessionOpInput :> Post '[JSON] ()

server :: ServerT Spi (RIO BasicContext)
server = createSession :<|> refreshSession
  :<|> revokeSession

mkClaims :: IO Jwt.ClaimsSet
mkClaims = do
  pure $ Jwt.emptyClaimsSet

jwtSign claims = runExceptT $ do
  jwk <- readPrivateKey "./jwt-key"
  case jwk of
    Nothing -> liftIO $ BS.putStr  "Lol!"
    Just key -> return $ Jwt.signClaims key (Jwt.newJWSHeader ((), alg)) claims

data SessionBuilderError = SessionBuilderError String

readPrivateKey :: FilePath -> ExceptT SessionBuilderError IO (Maybe PrivateKey)
readPrivateKey fp = do
  keys <- liftIO $ readKeyFile fp
  case keys of
    []        -> return Nothing
    x:_ -> return . extractPrivKey $ x
      where
        extractPrivKey :: PrivKey -> Maybe PrivateKey
        extractPrivKey (PrivKeyRSA k) = Just k
        extractPrivKey _              = Nothing
