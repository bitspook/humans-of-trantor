{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pms.Employee
  ( API
  , api
  )
where

import           Data.Pool                        (withResource)
import           Database.PostgreSQL.Simple       (Only (Only), query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Pms.Employee.Types               (API, Employee, ProjectName)
import           RIO                              hiding (Identity)
import           Servant                          (ServerT, err401)
import           Servant.Auth.Server              (AuthResult (..))
import           Types                            (App, AppContext (..))

getEmployees :: Maybe ProjectName -> App [Employee]
getEmployees project = do
  (AppContext pool _) <- ask
  let baseQuery' = [sql|
            SELECT DISTINCT on (payload->>'ecode')
              payload->>'ecode' as ecode,
              payload->>'name' as name,
              payload->>'email' as email,
              payload->>'project' as project,
              payload->>'designation' as designation
            FROM store.store
            WHERE name = 'DISCOVERED_EMPLOYEE'
            |]
      projectCond = [sql|AND  payload->>'project'=?|]
      queryEmployees conn = case project of
        Just p  -> query conn (mappend baseQuery' projectCond) (Only p)
        Nothing -> query conn baseQuery' ()
  liftIO $ withResource pool queryEmployees

api :: ServerT (API auth) App
api (Authenticated _) = getEmployees
api _                 = \_ -> throwM err401
