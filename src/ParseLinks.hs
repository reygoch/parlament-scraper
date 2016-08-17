module ParseLinks where

import Text.Parsec
import Text.Parsec.String

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 (pack)

import Control.Applicative ((*>), (<*))

colOP :: Parser String
colOP = string "<td class=\"liste\">"

colCL :: Parser String
colCL = string "</td>"

col :: Parser String
col = do
  colOP
  spaces
  result <- link <|> return ""
  anyChar `manyTill` colCL
  return result

href :: Parser String
href = do
  string "href=\""
  link <- anyChar `manyTill` (char '"')
  return link

linkOP :: Parser String
linkOP = string "<a"

linkCL :: Parser String
linkCL = string "</a>"

link :: Parser String
link = do
  spaces
  linkOP
  spaces
  result <- href
  anyChar `manyTill` linkCL
  return result

cols :: Parser [String]
cols = scan
  where scan = (eof >> return [])
               <|> do {r <- try col; rs <- scan; return (r:rs)}
               <|> do {anyChar; scan}

test1 :: String
test1 = "<td class=\"liste\">Hello world!</td>"

test2 :: String
test2 = read $ show $ pack test1

test3 :: String
test3 = "<html>asdfasd<td class=\"liste\">Hello world 1!</td>alskdj<td class=\"liste\">Hello world 2!</td>asldjfasldjf<td class=\"liste\">Hello world 3!</td>asdfsdf<td class=\"liste\">Hello world 4!</td>adsafasd"

test4 :: String
test4 = "asdfas<a href=\"some link data\">rest of the link</a>adsasdfsd"

parseMyTest :: String -> Either ParseError [String]
parseMyTest test = parse cols "test" test

btos :: ByteString -> String
btos = read . show