module Parsers where

import Text.Parsec
import Text.Parsec.String

data MemberLink = MemberLink
  { linkName :: String
  , linkUrl :: String
  }

emptyMember = MemberLink "" ""

instance Show MemberLink where
  show link = "(" ++ linkName link ++ " :: " ++ linkUrl link ++ ")"

example :: String
example = "<td class=liste><a href=#>Prezime, Ime</a></td>"

linkTest :: String
linkTest = "<a href=\"someLink\">Some Name</a>"

memberLinkTest :: String
memberLinkTest = "<td class=\"liste\">  <a href=\"someLink\">Some Name</a>  </td>"

memberLinksTest :: String
memberLinksTest = "asdfasdfas<><html><td class=\"liste\">  hello  </td>asdfsa<td class=\"liste\">  <a href=\"someLink\">Some Name</a>  </td><td class=\"liste\">  <a href=\"someLink\">Some Name</a>  </td>alaskdjfsadj"

--

parseLinks source html = parse memberLinks source html

memberLinks :: Parser [MemberLink]
memberLinks = scan
  where
    scan =
      (eof >> return [])
      <|> do {r <- try memberLink; rs <- scan; return (r:rs)}
      <|> do {anyChar; scan}

memberLink :: Parser MemberLink
memberLink = do
  string "<td class=\"liste\">"
  spaces
  result <- link
  anyChar `manyTill` (string "</td>")
  return result


-- <a href="url">Name</a>
link :: Parser MemberLink
link = do
  string "<a href=\""
  url <- anyChar `manyTill` (string "\"")

  anyChar `manyTill` (string ">")
  name <- anyChar `manyTill` (string "</a>")

  return $ MemberLink name url