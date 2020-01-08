module Session
  ( Api,
    server
  ) where
import           Data.Aeson
import           RIO
import qualified RIO.ByteString      as BS
import           Servant             as S
import           Servant.Auth        as SA
import           Servant.Auth.Server as SAS
import           Session.Insecure
import           Session.Secure
import           Session.Types

type Api auths =
  (SAS.Auth auths Session :> SecureEndpoints)
  :<|> InsecureEndpoints

server :: CookieSettings -> JWTSettings -> Server (Api auths)
server cs jwts = refreshSession :<|> createSession cs jwts
