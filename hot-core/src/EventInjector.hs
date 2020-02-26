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
import           Pms.Standup                        (StandupBody, StandupId)
import           RIO                                hiding (id)
import           Types                              (Date, Email)

data StandupType = Committed | Delivered | Impediment deriving (Show, Generic)

instance FromJSON StandupType where
  parseJSON (String s) = case s of
    "committed"  -> return Committed
    "delivered"  -> return Delivered
    "impediment" -> return Impediment
    _ -> fail "Invalid standup type. Valid values: [ committed, delivered, impediment ]"
  parseJSON _ = fail "Invalid standup type. Valid values: [ committed, delivered, impediment ]"

instance ToJSON StandupType where
  toJSON st = case st of
    Committed  -> String "committed"
    Delivered  -> String "delivered"
    Impediment -> String "impediment"

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

-- ReceivedStandupUpdate
data ReceivedStandupUpdate = ReceivedStandupUpdate
  { ecode       :: Ecode
  , project     :: ProjectName
  , standupType :: StandupType
  , date        :: Date
  , standup     :: StandupBody
  } deriving (Show, Generic)

instance ToField ReceivedStandupUpdate where
  toField = toJSONField

instance FromJSON ReceivedStandupUpdate where
  parseJSON = withObject "event" $ \o ->
    ReceivedStandupUpdate
    <$> o .: "ecode"
    <*> o .: "project"
    <*> o .: "type"
    <*> o .: "date"
    <*> o .: "standup"

instance ToJSON ReceivedStandupUpdate where
  toJSON p = object
    [ "ecode" .= ecode (p :: ReceivedStandupUpdate)
    , "project" .= project (p :: ReceivedStandupUpdate)
    , "type" .= standupType (p :: ReceivedStandupUpdate)
    , "date" .= date (p :: ReceivedStandupUpdate)
    , "standup" .= standup (p :: ReceivedStandupUpdate)
    ]
---

-- ReceivedStandupUpdateV2
data ReceivedStandupUpdateV2 = ReceivedStandupUpdateV2
  { source      :: Maybe StandupId
  , ecode       :: Ecode
  , project     :: ProjectName
  , standup     :: StandupBody
  , date        :: Date
  , isDelivered :: Bool
  , issues      :: [UUID]
  , notes       :: [UUID]
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
    <*> o .: "notes"
    <*> o .: "issues"

instance ToJSON ReceivedStandupUpdateV2 where
  toJSON p = object
    [ "source" .= source (p :: ReceivedStandupUpdateV2)
    , "ecode" .= ecode (p :: ReceivedStandupUpdateV2)
    , "project" .= project (p :: ReceivedStandupUpdateV2)
    , "standup" .= standup (p :: ReceivedStandupUpdateV2)
    , "date" .= date (p :: ReceivedStandupUpdateV2)
    , "isDelivered" .= isDelivered (p :: ReceivedStandupUpdateV2)
    , "issues" .= issues (p :: ReceivedStandupUpdateV2)
    , "notes" .= notes (p :: ReceivedStandupUpdateV2)
    ]
---
