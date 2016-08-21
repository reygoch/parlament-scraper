module TemplateWriters where

import Data.Char (toLower)
import Parsers (Profile(..))
import System.Directory (createDirectoryIfMissing, copyFile)

import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.UTF8 (fromString)

import GetHtml
import Parsers

newScrape :: String -> String -> IO ()
newScrape listUrl name = do
  list <- getProfileLinks listUrl
  profiles <- mapM (getProfile . (baseUrl++)) list

  createDirectoryIfMissing True (dir ++ "/css")
  createDirectoryIfMissing True (dir ++ "/img")

  copyFile "template/css/index.css" (dir ++ "/css/index.css")
  copyFile "template/index.html" (dir ++ "/index.html")

  mapM_ (BS.appendFile (dir ++ "/index.html") . fromString . profileTemplate) profiles

  end <- readFile "template/template-end.html"
  appendFile (dir ++ "/index.html") end

  mapM_ (saveProfileImage dir) profiles

  where
    dir = "scrapes/" ++ name


















--

baseUrl :: String
baseUrl = "http://www.sabor.hr/"

saziv :: String
saziv = baseUrl ++ "zastupnici"

--

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

saveImage :: String -> String -> IO ()
saveImage path url = getImage url >>= BS.writeFile path

saveProfileImage :: String -> Profile -> IO ()
saveProfileImage path p = saveImage (path ++ "/img/" ++ (imageName p) ++ ".jpg") (baseUrl ++ (imageLink p))

--

profileTemplate :: Profile -> String
profileTemplate p = concat
  [ "<div class='profile col06 row'>"
  , "<div class='image col04'>"
  , "<img src='img/", imageName p, ".jpg' alt='", imageName p, "'>"
  , "</div>"
  , "<div class='info col06'>"
  , "<h4 class='name'>"
  , name p, " ", surname p, " "
  , "<span class='party'>", party p, "</span>"
  , "</h4>"
  , "<p>", description p, "</p>"
  , "</div>"
  , "</div>"
  ]

repl :: Char -> Char
repl 'č' = 'c'
repl 'ć' = 'c'
repl 'đ' = 'd'
repl 'š' = 's'
repl ' ' = '-'
repl c   =  c

imageName :: Profile -> String
imageName p = map (repl . toLower) (name p ++ " " ++ (surname p))