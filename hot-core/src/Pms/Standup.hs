{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Pms.Standup where

import           Data.Aeson
import           Data.UUID
import           Database.PostgreSQL.Simple.FromField (FromField (..),
                                                       fromJSONField)
import           Database.PostgreSQL.Simple.FromRow   (FromRow (..), field)
import           Database.PostgreSQL.Simple.ToField   (Action (..),
                                                       ToField (..))
import           Pms.Employee.Types                   (Ecode, ProjectName)
import           RIO                                  hiding (id)
import           Types                                (Date, fromTextField)

-- StandupId
newtype StandupId = StandupId UUID deriving (Show, Generic, ToJSON, FromJSON)

instance FromField StandupId where
  fromField = fromJSONField
---

-- StandupBody
newtype StandupBody = StandupBody Text deriving (Show, Generic, ToJSON, FromJSON)

instance FromField StandupBody where
  fromField = fromTextField StandupBody

instance ToField StandupBody where
  toField (StandupBody a) = Escape $ encodeUtf8 a
---

data StandupData = StandupData
  { ecode       :: Ecode
  , project     :: ProjectName
  , standup     :: StandupBody
  , date        :: Date
  , isDelivered :: Bool
  } deriving (Show, Generic, ToJSON, FromJSON, FromRow)

data Standup = Standup StandupId StandupData deriving (Show)

instance ToJSON Standup where
  toJSON (Standup id StandupData {..} ) = object
    [ "id" .= id
    , "ecode" .= ecode
    , "project" .= project
    , "standup" .= standup
    , "date" .= date
    , "isDelivered" .= isDelivered
    ]

instance FromJSON Standup where
  parseJSON = withObject "standup" $ \o -> do
    id <- o .: "id"
    ecode <- o .: "ecode"
    project <- o .: "project"
    standup <- o .: "standup"
    date <- o .: "date"
    isDelivered <- o .: "isDelivered"
    return $ Standup id StandupData{..}

instance FromRow Standup where
  fromRow = do
    id <- field
    dat <- StandupData <$> field <*> field <*> field <*> field <*> field
    return $ Standup id dat
