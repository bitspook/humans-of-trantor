{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Types where

import           Data.Aeson
import           Data.Pool
import           Data.Text                            as T
import           Data.Text.Encoding                   (decodeUtf8)
import           Data.Time.Format
import           Data.Time.LocalTime
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
newtype Date = Date LocalTime deriving (Show, Generic)

instance FromJSON Date where
  parseJSON (String d) = case parseTimeM False defaultTimeLocale "%Y-%m-%d" (T.unpack d) of
    Just t  -> return $ Date t
    Nothing -> fail "Invalid Date. Date should be in format 'YYYY-MM-DD'"
  parseJSON _ = fail "Invalid Date. Date should be a string in format 'YYYY-MM-DD'"

instance ToJSON Date where
  toJSON (Date d) = fromString $ formatTime defaultTimeLocale "%Y-%m-%d" d

instance ToField Date where
  toField (Date d) = Escape $ fromString $ formatTime defaultTimeLocale "%Y-%m-%d" d

instance FromField Date where
  fromField f dat = case dat of
    Just b  -> case parseTimeM False defaultTimeLocale "%Y-%m-%d" $ T.unpack $ decodeUtf8 b of
      Just t  -> return $ Date t
      Nothing -> returnError ConversionFailed f (show dat)
    Nothing -> returnError ConversionFailed f (show dat)
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
