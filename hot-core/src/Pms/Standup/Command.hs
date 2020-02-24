{-# LANGUAGE RecordWildCards #-}
module Pms.Standup.Command where

import           Data.UUID
import           EventInjector         (ReceivedStandupUpdateV2 (..))
import           EventInjector.Command (receivedStandupUpdateEventV2)
import           Pms.Standup
import           RIO
import           Types

addStandup :: StandupData -> App UUID
addStandup StandupData{..} = receivedStandupUpdateEventV2 event
  where
    event = ReceivedStandupUpdateV2 Nothing ecode project standup date False [] []
