module Session
  ( API
  , server
  )
where

import           Data.Aeson
import           RIO
import           Servant
import           Servant.Auth.Server
import qualified Session.Insecure              as Insecure
import qualified Session.Secure                as Secure
import           Session.Types

server :: CookieSettings -> JWTSettings -> Server (API auths)
server cs jwts = Secure.server :<|> Insecure.server cs jwts

type API auths = (Auth auths Session :> Secure.API) :<|> Insecure.API
