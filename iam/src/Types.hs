module Types where

import           Data.Aeson (FromJSON, ToJSON)
import           RIO

newtype UUID = UUID Text deriving (Show, Generic, ToJSON, FromJSON)
newtype Email = Email Text deriving (Show, Generic, ToJSON, FromJSON)
