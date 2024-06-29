module Comm(encode, communicate) where

import Term(toICFP)
import Base94(s2icfp)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header(Header, hAuthorization)
import Network.HTTP.Types.Method(methodPost)
import Data.ByteString.UTF8 as BSU 
import Data.ByteString.Lazy as BL 
import Data.ByteString.Lazy.UTF8 as BLU 

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding as TSE

authToken :: Header
authToken = (hAuthorization , BSU.fromString "Bearer a57888ca-06a7-492c-8439-8c90dbc183dd")

commUrl :: String
commUrl = "https://boundvariable.space/communicate"

encode :: Text->BSU.ByteString
encode s =  BSU.fromString $ 'S' : (s2icfp $ Text.unpack s)

communicate :: Text -> IO Text
communicate s = do
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest commUrl
    let request = initialRequest { method = methodPost, requestBody = RequestBodyBS $ encode s, requestHeaders = [authToken] }
    response <- httpLbs request manager
    return $ TSE.decodeLatin1 . BL.toStrict $ responseBody response
