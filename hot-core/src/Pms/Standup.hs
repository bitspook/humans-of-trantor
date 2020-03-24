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
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow   (FromRow (..), field)
import           Database.PostgreSQL.Simple.ToField
import           Pms.Employee.Types                   (Ecode, ProjectName)
import           RIO                                  hiding (id)
import           Servant                              (FromHttpApiData (..))
import           Types                                (Date, Timestamps (..),
                                                       fromIntField,
                                                       fromTextField,
                                                       fromUUIDField)

-- StandupId
newtype StandupId = StandupId UUID deriving (Show, Generic, ToJSON, FromJSON)

instance FromField StandupId where
  fromField = fromUUIDField StandupId

instance ToField StandupId where
  toField (StandupId s) = Escape $ encodeUtf8 $ toText s

instance FromHttpApiData StandupId where
  parseQueryParam sid = suid
   where
    suid = case StandupId <$> fromText sid of
      Just suid' -> Right suid'
      Nothing    -> Left "Invalid Standup Id"
---

-- StandupBody
newtype StandupBody = StandupBody Text deriving (Show, Generic, ToJSON, FromJSON)

instance FromField StandupBody where
  fromField = fromTextField StandupBody

instance ToField StandupBody where
  toField (StandupBody a) = Escape $ encodeUtf8 a
---

-- StandupPriority
newtype StandupPriority = StandupPriority Int deriving (Show, Generic, ToJSON, FromJSON)

instance FromField StandupPriority where
  fromField = fromIntField StandupPriority

instance ToField StandupPriority where
  toField = toJSONField
---

data StandupData = StandupData
  { ecode       :: Ecode
  , project     :: ProjectName
  , standup     :: StandupBody
  , date        :: Date
  , isDelivered :: Bool
  , priority    :: StandupPriority
  } deriving (Show, Generic, ToJSON, FromJSON, FromRow)

data Standup = Standup StandupId StandupData Timestamps deriving (Show)

instance ToJSON Standup where
  toJSON (Standup id StandupData {..} Timestamps {..}) = object
    [ "id" .= id
    , "ecode" .= ecode
    , "project" .= project
    , "standup" .= standup
    , "date" .= date
    , "isDelivered" .= isDelivered
    , "priority" .= priority
    , "createdAt" .= createdAt
    , "updatedAt" .= updatedAt
    ]

instance FromJSON Standup where
  parseJSON = withObject "standup" $ \o -> do
    id          <- o .: "id"
    ecode       <- o .: "ecode"
    project     <- o .: "project"
    standup     <- o .: "standup"
    date        <- o .: "date"
    isDelivered <- o .: "isDelivered"
    priority    <- o .: "priority"
    createdAt <- o .: "createdAt"
    updatedAt <- o .: "updatedAt"
    return $ Standup id StandupData { .. } Timestamps {..}

instance FromRow Standup where
  fromRow = do
    id  <- field
    dat <-
      StandupData <$> field <*> field <*> field <*> field <*> field <*> field
    ts <- Timestamps <$> field <*> field
    return $ Standup id dat ts
