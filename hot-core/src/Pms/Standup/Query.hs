{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Pms.Standup.Query where

import           Data.Pool                        (withResource)
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Database.PostgreSQL.Simple       (Only (Only), query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Pms.Employee.Types               (Ecode)
import           Pms.Standup                      (Standup, StandupId)
import           RIO                              hiding (Identity)
import           Servant
import           Types                            (App, AppContext (..),
                                                   Date (..))

getStandups :: Maybe Ecode -> Maybe Date -> Maybe Date -> App [Standup]
getStandups ecode' before' after' = do
  (AppContext pool _) <- ask
  now <- liftIO getCurrentTime
  let
    before = fromMaybe (Date $ utcToLocalTime utc now) before'
    after = fromMaybe (Date $ utcToLocalTime utc now) after'
    baseQuery = [sql|SELECT * from standup WHERE DATE(date) < ? AND DATE(date) > ?|]
    ecodeCond = [sql| AND ecode=?|]
    finalQuery = [sql| LIMIT 1000|]
  -- FIXME: WHAT THE F
    queryStandups conn = case ecode' of
      Nothing ->
        query conn (mconcat [baseQuery, finalQuery]) (before, after)
      Just ecode ->
        query conn (mconcat [baseQuery, ecodeCond, finalQuery]) (before, after, ecode)
  liftIO $ withResource pool queryStandups

standup :: StandupId -> App Standup
standup sid = do
  (AppContext pool _) <- ask
  liftIO $ withResource pool $ \conn -> do
    res :: [Standup] <- query conn [sql|SELECT * FROM standup WHERE id = ?|] (Only sid)
    case res of
      [s] -> return s
      _   -> throwM err400
