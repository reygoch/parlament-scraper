module Main where

import GetHtml
import Parsers
import Text.Parsec (parse)
import Text.Parsec.String (Parser)

main :: IO ()
main = do
  list <- getProfileLinks saziv
  profiles <- mapM (getProfile . (baseUrl++)) list
  mapM_ (putStrLn . name) profiles


baseUrl :: String
baseUrl = "http://www.sabor.hr/"

saziv :: String
saziv = baseUrl ++ "zastupnici"

getProfileLinks :: String -> IO [String]
getProfileLinks = parseUrl entries []

getProfile :: String -> IO Profile
getProfile = parseUrl profile emptyProfile

testProfile :: String -> IO ()
testProfile url = do
  html <- getHtml url
  putStrLn $ show $ parse profile url html

parseUrl :: Parser a -> a -> String -> IO a
parseUrl parser empty url = do
  html <- getHtml url
  case (parse parser url html) of
    Left  _ -> return empty
    Right r -> return r