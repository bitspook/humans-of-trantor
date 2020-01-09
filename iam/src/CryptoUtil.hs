module CryptoUtil (readPemRsaKey) where

import qualified Crypto.PubKey.OpenSsh   as D
import           Crypto.PubKey.RSA.Types
import qualified Crypto.Types.PubKey.RSA as CRSA
import           RIO
import qualified RIO.ByteString          as B

data PrivateKeyError = NotRSA | ParseErr String | NoKey FilePath

-- | Read private RSA Key in PEM format
readPemRsaKey
  :: MonadIO m
  => FilePath -- ^ file path to read key from; must be PEM
  -> m (Either PrivateKeyError PrivateKey)
readPemRsaKey path = do
  eKey <- liftIO $ D.decodePrivate <$> B.readFile path
  return $ case eKey of
    Right (D.OpenSshPrivateKeyRsa k) -> Right . convertPri' $ k
    Right other                      -> Left NotRSA
    Left  err                        -> Left . ParseErr $ err

convertPub' :: CRSA.PublicKey -> PublicKey
convertPub' (CRSA.PublicKey s n e) = PublicKey s n e

convertPri' :: CRSA.PrivateKey -> PrivateKey
convertPri' pk = PrivateKey
  { private_pub  = convertPub' . CRSA.private_pub $ pk
  , private_d    = CRSA.private_d pk
  , private_p    = CRSA.private_p pk
  , private_q    = CRSA.private_q pk
  , private_dP   = CRSA.private_dP pk
  , private_dQ   = CRSA.private_dQ pk
  , private_qinv = CRSA.private_qinv pk
  }
