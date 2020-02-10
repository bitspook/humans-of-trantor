{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api
  ( run
  )
where
import           Crypto.JOSE                 (fromRSA)
import           CryptoUtil                  (readPemRsaKey)
import           Data.Either.Combinators     (fromRight')
import           Data.Pool                   (withResource)
import           Db                          (initConnectionPool, migrate)
import           Dhall                       (auto, input)
import qualified Iam                         (app)
import           Network.Wai                 (Middleware)
import qualified Network.Wai.Handler.Warp    as Warp (run)
import           Network.Wai.Middleware.Cors (cors, corsRequestHeaders,
                                              simpleCorsResourcePolicy)
import           RIO.Text                    (encodeUtf8, unpack)
import           Types

corsMiddleware :: Middleware
corsMiddleware = cors ( const $ Just (simpleCorsResourcePolicy  { corsRequestHeaders = ["Content-Type"] }) )

run :: IO ()
run = do
  conf       <- input auto "./config.dhall"
  pool       <- initConnectionPool . encodeUtf8 . dbUrl $ conf
  _          <- withResource pool $ migrate (unpack . migrationsDir $ conf)
  privateKey <- fromRSA . fromRight' <$> readPemRsaKey (unpack . jwtKeysPath $ conf)
  Warp.run (fromIntegral $ port conf)
    $ corsMiddleware (Iam.app privateKey pool)
