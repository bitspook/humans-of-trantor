{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Api where
import           Control.Monad.Trans.Except            (ExceptT (..))
import           CryptoUtil                            (jwtSettings)
import           Data.Pool                             (withResource)
import           Db                                    (initConnectionPool,
                                                        migrate)
import           Dhall                                 (auto, input)
import qualified EventInjector.Api                     as EventInjector (API,
                                                                         api)
import qualified Iam                                   (API, api)
import           Network.Wai                           (Middleware)
import qualified Network.Wai.Handler.Warp              as Warp (run)
import           Network.Wai.Middleware.Cors           (cors,
                                                        corsRequestHeaders,
                                                        simpleCorsResourcePolicy)
import           Network.Wai.Middleware.RequestLogger  (logStdoutDev)
import           Network.Wai.Middleware.Servant.Errors (errorMw)
import qualified Pms                                   (API, api)
import           RIO
import           RIO.Text                              (encodeUtf8, unpack)
import           Servant                               hiding (runHandler)
import           Servant.Auth.Server                   (JWT, JWTSettings,
                                                        defaultCookieSettings)
import           Types

corsMw :: Middleware
corsMw = cors $ const $ Just $ simpleCorsResourcePolicy
  { corsRequestHeaders = ["Content-Type", "Authorization"]
  }

runHandler :: c -> RIO c a -> Servant.Handler a
runHandler ctx a = Servant.Handler $ ExceptT $ try $ runReaderT (unRIO a) ctx

app
  :: forall c x w
   . (HasServer x w)
  => Proxy x
  -> Context w
  -> c
  -> ServerT x (RIO c)
  -> Application
app proxy sctx ctx actions = serveWithContext proxy sctx $ srv ctx
  where srv c = hoistServerWithContext proxy (Proxy @w) (runHandler c) actions

type API auths = Iam.API :<|> Pms.API auths :<|> EventInjector.API auths

proxyApi :: Proxy (API '[JWT])
proxyApi = Proxy

api :: JWTSettings -> ServerT (API auths) App
api jwts = Iam.api jwts :<|> Pms.api :<|> EventInjector.api

run :: IO ()
run = do
  conf <- input auto "./config.dhall"
  pool <- initConnectionPool . encodeUtf8 . dbUrl $ conf
  _    <- withResource pool $ migrate (unpack . migrationsDir $ conf)
  jwts <- jwtSettings (unpack . jwtKeysPath $ conf)

  let sctx = defaultCookieSettings :. jwts :. EmptyContext
      ctx  = AppContext pool conf

  Warp.run (fromIntegral $ port conf)
    $ corsMw
    $ errorMw @JSON @'["error", "status"]
    $ logStdoutDev
    $ app proxyApi sctx ctx (api jwts)
