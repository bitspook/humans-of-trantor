{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Types where

import           Data.Aeson                           (FromJSON, ToJSON)
import           Data.Pool
import           Data.Text.Encoding                   (decodeUtf8)
import           Database.PostgreSQL.Simple           (Connection)
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.Internal  (Conversion, Field)
import           Database.PostgreSQL.Simple.ToField
import           Dhall                                (Interpret)
import           RIO

fromTextField
  :: Typeable a =>
     (Text -> a)
     -> Field
     -> Maybe ByteString
     -> Conversion a
fromTextField constructor f dat = case dat of
    Just b  -> return $ constructor $ decodeUtf8 b
    Nothing -> returnError ConversionFailed f (show dat)

-- Email
newtype Email = Email Text deriving (Show, Generic, ToJSON, FromJSON)

instance FromField Email where
  fromField = fromTextField Email

instance ToField Email where
  toField (Email a) = Escape $ encodeUtf8 a
---

-- Date
newtype Date = Date Text deriving (Show, Generic, FromJSON, ToJSON)

instance ToField Date where
  toField (Date d) = Escape $ encodeUtf8 d

instance FromField Date where
  fromField = fromTextField Date
---

data Config = Config
  { dbUrl         :: Text,
    jwtKeysPath   :: Text,
    migrationsDir :: Text,
    port          :: Natural
  } deriving (Generic, Show)

instance Interpret Config

data AppContext = AppContext { dbPool :: Pool Connection,  config :: Config }

type App = RIO AppContext
