{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module EventInjector where

import           Data.Aeson
import           Data.UUID                          (UUID)
import           Database.PostgreSQL.Simple.ToField (ToField (..), toJSONField)
import           Pms.Employee.Types                 (Designation, Ecode,
                                                     EmployeeName, ProjectName)
import           Pms.Standup                        (StandupBody, StandupId,
                                                     StandupPriority)
import           RIO                                hiding (id)
import           Types                              (Date, Email)

-- DiscoveredEmployee
data DiscoveredEmployee = DiscoveredEmployee
  { email       :: Email
  , ecode       :: Ecode
  , name        :: EmployeeName
  , project     :: Maybe ProjectName
  , designation :: Designation
  } deriving (Show, Generic, FromJSON, ToJSON)

instance ToField DiscoveredEmployee where
  toField = toJSONField
---

-- DiscoveredProject
data DiscoveredProject = DiscoveredProject
  { id      :: UUID
  , project :: ProjectName
  } deriving (Show, Generic, FromJSON, ToJSON)

instance ToField DiscoveredProject where
  toField = toJSONField
---

-- ReceivedStandupUpdateV2
data ReceivedStandupUpdateV2 = ReceivedStandupUpdateV2
  { source      :: Maybe StandupId
  , ecode       :: Ecode
  , project     :: ProjectName
  , standup     :: StandupBody
  , date        :: Date
  , isDelivered :: Bool
  , priority    :: StandupPriority
  } deriving (Show, Generic)

instance ToField ReceivedStandupUpdateV2 where
  toField = toJSONField

instance FromJSON ReceivedStandupUpdateV2 where
  parseJSON = withObject "payload" $ \o ->
    ReceivedStandupUpdateV2
    <$> o .: "source"
    <*> o .: "ecode"
    <*> o .: "project"
    <*> o .: "standup"
    <*> o .: "date"
    <*> o .: "isDelivered"
    <*> o .: "priority"

instance ToJSON ReceivedStandupUpdateV2 where
  toJSON p = object
    [ "source" .= source (p :: ReceivedStandupUpdateV2)
    , "ecode" .= ecode (p :: ReceivedStandupUpdateV2)
    , "project" .= project (p :: ReceivedStandupUpdateV2)
    , "standup" .= standup (p :: ReceivedStandupUpdateV2)
    , "date" .= date (p :: ReceivedStandupUpdateV2)
    , "isDelivered" .= isDelivered (p :: ReceivedStandupUpdateV2)
    , "priority" .= priority (p :: ReceivedStandupUpdateV2)
    ]
---

newtype DeleteStandupUpdate = DeleteStandupUpdate
  { source :: StandupId
  } deriving (Show, Generic, FromJSON, ToJSON)

instance ToField DeleteStandupUpdate where
  toField = toJSONField
