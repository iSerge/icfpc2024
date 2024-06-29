module Comm (encode, communicate) where

import Base94 (s2icfp)
import Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 as BSU
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding as TSE
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header (Header, hAuthorization)
import Network.HTTP.Types.Method (methodPost)
import Secret (authToken)

authorization :: Header
authorization = (hAuthorization, BSU.fromString authToken)

commUrl :: String
commUrl = "https://boundvariable.space/communicate"

encode :: Text -> BSU.ByteString
encode s = BSU.fromString $ 'S' : s2icfp (Text.unpack s)

communicate :: Text -> IO Text
communicate s = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest commUrl
  let request = initialRequest {method = methodPost, requestBody = RequestBodyBS $ encode s, requestHeaders = [authorization]}
  response <- httpLbs request manager
  return $ TSE.decodeUtf8 . BL.toStrict $ responseBody response
