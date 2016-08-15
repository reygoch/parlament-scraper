module Main where

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.ByteString.Lazy as BS

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar)
import Text.Parsec.Char
import Data.Char
import Text.Parsec.Combinator (many1)

saziv :: String
saziv = "http://www.sabor.hr/zastupnici"

main :: IO ()
main = print "hello"

getHtml :: IO ()
getHtml = do
  man <- newManager defaultManagerSettings

  req <- parseRequest saziv
  res <- httpLbs req man

  BS.writeFile "test.html" $ responseBody res

-- Parsec Stuff

type Link = String
type LinkList = [Link]

testHtml :: String
testHtml = "alskjfalskdja<td class=\"liste\">some result</td><td class=\"liste\">some result</td>alsklasfjasd<td class=\"liste\">some result</td>alksjfaksdjf<td class=\"liste\">some result</td>"

colOP :: Parser String
colOP = string "<td class=\"liste\">"

colCL :: Parser String
colCL = string "</td>"

col :: Parser String
col = do
  anyChar `manyTill` colOP
  content <- anyChar `manyTill` colCL
  return content

cols :: Parser [String]
cols = do
  many col