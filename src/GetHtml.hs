module GetHtml (URL, getHtml, btos) where

import Network.HTTP.Client
import Data.ByteString.Lazy (ByteString)

type URL = String

getHtml :: URL -> IO ByteString
getHtml url = do
  man <- newManager defaultManagerSettings

  req <- parseRequest url
  res <- httpLbs req man

  return $ responseBody res

btos :: ByteString -> String
btos = read . show