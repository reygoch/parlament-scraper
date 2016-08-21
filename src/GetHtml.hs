module GetHtml (URL, getHtml, getImage) where

import Network.HTTP.Client
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (toString)

type URL = String

getHtml :: URL -> IO String
getHtml url = do
  man <- newManager defaultManagerSettings

  req <- parseRequest url
  res <- httpLbs req man

  return $ toString $ responseBody res

getImage :: String -> IO ByteString
getImage url = do
  man <- newManager defaultManagerSettings

  req <- parseRequest url
  res <- httpLbs req man

  return $ responseBody res