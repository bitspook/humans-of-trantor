{-# LANGUAGE RecordWildCards #-}
module Pms.Standup.Command where

import           Data.Coerce           (coerce)
import           EventInjector         (ReceivedStandupUpdateV2 (..))
import           EventInjector.Command (receivedStandupUpdateEventV2)
import           Pms.Standup
import           RIO
import           Types

addStandup :: StandupData -> App Standup
addStandup StandupData{..} = do
  let event = ReceivedStandupUpdateV2 Nothing ecode project standup date isDelivered priority
  eventId <- receivedStandupUpdateEventV2 event
  return $ Standup (coerce eventId) StandupData{..}

replaceStandup :: StandupId -> StandupData -> App Standup
replaceStandup sid dat@StandupData{..} = do
  let event = ReceivedStandupUpdateV2 (Just sid) ecode project standup date isDelivered priority
  _ <- receivedStandupUpdateEventV2 event
  return $ Standup sid dat
