module Types where

import           Data.Aeson                           (FromJSON, ToJSON)
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           RIO

newtype Email = Email Text deriving (Show, Generic, ToJSON, FromJSON)

instance FromField Email where
  fromField = fromJSONField

instance ToField Email where
  toField = toJSONField
