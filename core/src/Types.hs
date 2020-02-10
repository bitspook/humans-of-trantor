{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import           Data.Aeson                           (FromJSON, ToJSON)
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Dhall                                (Interpret)
import           RIO

newtype Email = Email Text deriving (Show, Generic, ToJSON, FromJSON)

instance FromField Email where
  fromField = fromJSONField

instance ToField Email where
  toField = toJSONField

data Config = Config
  { dbUrl         :: Text,
    jwtKeysPath   :: Text,
    migrationsDir :: Text,
    port          :: Natural
  } deriving (Generic, Show)

instance Interpret Config
