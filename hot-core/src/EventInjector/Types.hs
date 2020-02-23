{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module EventInjector.Types where

import           Data.Aeson
import           Data.UUID                          (UUID)
import           Database.PostgreSQL.Simple.ToField (Action (Escape),
                                                     ToField (..), toJSONField)
import           Iam.Session.Types                  (AccessToken)
import           Pms.Employee.Types                 (Designation, Ecode,
                                                     EmployeeName, ProjectName)
import           RIO
import           Servant
import           Servant.Auth.Server
import           Types                              (Email)

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

-- EventVersion
data EventVersion = V1 deriving (Show)

instance FromJSON EventVersion where
  parseJSON (String "v1") = return V1
  parseJSON _             = fail "Invalid Event version. Valid values: [ v1 ]"

instance ToField EventVersion where
  toField V1 = Escape $ encodeUtf8 "v1"
---

-- Standup
newtype Standup = Standup Text deriving (Show, Generic, FromJSON, ToJSON)

instance ToField Standup where
  toField (Standup s) = Escape $ encodeUtf8 s
---

-- Date
newtype Date = Date Text deriving (Show, Generic, FromJSON, ToJSON)

instance ToField Date where
  toField (Date d) = Escape $ encodeUtf8 d
---

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
  , standup     :: Standup
  } deriving (Show, Generic)

instance ToField ReceivedStandupUpdate where
  toField = toJSONField

instance FromJSON ReceivedStandupUpdate where
  parseJSON = withObject "event" $ \o ->
    ReceivedStandupUpdate
    <$> o .: "ecode"
    <*> o.: "project"
    <*> o.: "type"
    <*> o.: "date"
    <*> o.: "standup"

instance ToJSON ReceivedStandupUpdate where
  toJSON p = object
    [ "ecode" .= ecode (p :: ReceivedStandupUpdate)
    , "project" .= project (p :: ReceivedStandupUpdate)
    , "type" .= standupType p
    , "date" .= date p
    , "standup" .= standup p
    ]
---

data Event p = Event
  { version :: EventVersion
  , payload :: p
  } deriving (Show, Generic, FromJSON)

newtype DiscoveredEmployeeEvent = DiscoveredEmployeeEvent (Event DiscoveredEmployee)
  deriving (Show, Generic, FromJSON)

newtype DiscoveredProjectEvent = DiscoveredProjectEvent (Event DiscoveredProject)
  deriving (Show, Generic, FromJSON)

newtype ReceivedStandupUpdateEvent = ReceivedStandupUpdateEvent (Event ReceivedStandupUpdate)
  deriving (Show, Generic, FromJSON)

type API auths =  Auth auths AccessToken :>
  "event" :> (
  "DISCOVERED_EMPLOYEE" :> ReqBody '[JSON] DiscoveredEmployeeEvent :> Post '[JSON] NoContent
    :<|> "DISCOVERED_PROJECT" :> ReqBody '[JSON] DiscoveredProjectEvent :> Post '[JSON] NoContent
    :<|> "RECEIVED_STANDUP_UPDATE" :> ReqBody '[JSON] ReceivedStandupUpdateEvent :> Post '[JSON] NoContent
  )
