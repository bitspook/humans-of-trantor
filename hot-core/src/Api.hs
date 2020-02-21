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
import qualified Iam                                   (API, server)
import           Network.Wai                           (Middleware)
import qualified Network.Wai.Handler.Warp              as Warp (run)
import           Network.Wai.Middleware.Cors           (cors,
                                                        corsRequestHeaders,
                                                        simpleCorsResourcePolicy)
import           Network.Wai.Middleware.RequestLogger  (logStdoutDev)
import           Network.Wai.Middleware.Servant.Errors (errorMw)
import           RIO
import           RIO.Text                              (encodeUtf8, unpack)
import           Servant                               hiding (runHandler)
import           Servant.Auth.Server                   (JWTSettings,
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
app api sctx ctx actions = serveWithContext api sctx $ srv ctx
  where srv c = hoistServerWithContext api (Proxy @w) (runHandler c) actions

proxyApi :: Proxy (API auth)
proxyApi = Proxy

type API auth = Iam.API auth
server :: JWTSettings -> ServerT (API auth) App
server jwts = Iam.server jwts

run :: IO ()
run = do
  conf <- input auto "./config.dhall"
  pool <- initConnectionPool . encodeUtf8 . dbUrl $ conf
  _    <- withResource pool $ migrate (unpack . migrationsDir $ conf)
  jwts <- jwtSettings (unpack . jwtKeysPath $ conf)

  let sctx = defaultCookieSettings :. jwts :. EmptyContext
      ctx  = AppContext conf pool

  Warp.run (fromIntegral $ port conf)
    $ corsMw
    $ errorMw @JSON @'["error", "status"]
    $ logStdoutDev
    $ app proxyApi sctx ctx (server jwts)
