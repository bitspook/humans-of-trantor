{-# LANGUAGE RecordWildCards #-}
module Pms.Standup.Command where

import           Data.Coerce           (coerce)
import           EventInjector         (DeleteStandupUpdate (..),
                                        ReceivedStandupUpdateV2 (..))
import           EventInjector.Command
import           Pms.Standup
import qualified Pms.Standup.Query     as Query
import           RIO
import           Servant
import           Types

addStandup :: StandupData -> App Standup
addStandup StandupData{..} = do
  let event = ReceivedStandupUpdateV2 Nothing ecode project standup date isDelivered priority
  (eventId, ts) <- receivedStandupUpdateEventV2 event
  return $ Standup (coerce eventId) StandupData{..} ts

replaceStandup :: StandupId -> StandupData -> App Standup
replaceStandup sid dat@StandupData{..} = do
  let event = ReceivedStandupUpdateV2 (Just sid) ecode project standup date isDelivered priority
  (_, ts) <- receivedStandupUpdateEventV2 event
  return $ Standup sid dat ts

deleteStandup :: StandupId -> App NoContent
deleteStandup sid = do
  _ <- Query.standup sid
  let event = DeleteStandupUpdate sid
  _ <- deleteStandupUpdateEvent event

  return NoContent
