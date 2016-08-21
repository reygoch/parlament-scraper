module Parsers where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Data.Char
--import Data.List
--import Data.List.Split
import Data.List.Extra

-- Parsers for list of MPs
-- example entry : <td class="liste"><a href="#targetItem">Some Text</a></td>

entries :: Parser [String]
entries = (eof >> return [])
  <|> do {e <- try entry; es <- entries; return (e:es)}
  <|> do {anyChar; entries}

--

entry :: Parser String
entry = do
  string "<td class=\"liste\">"
  url <- link
  anyChar `manyTill` (string "</td>")
  return url

entryTest :: String
entryTest = "<td class=\"liste\"><a href=\"lgs.axd?t=24&amp;id=6176\">Anušić,  Ivan</a>\n(HDZ)</td>"

--

link :: Parser String
link = do
  string "<a href=\""
  url <- anyChar `manyTill` (char '"')
  anyChar `manyTill` (string "</a>")
  return $ replace "&amp;" "&" url

linkTest :: String
linkTest = "<a href=\"url\">some text</a>"

-- Parsers for MP profile
data Profile = Profile
  { name :: String
  , surname :: String
  , party :: String
  , imageLink :: String
  , description :: String
  } deriving Show

emptyProfile = Profile "" "" "" "" ""

--

profile :: Parser Profile
profile = do
  anyChar `manyTill` (try $ lookAhead fullName)
  (pName, pSurname) <- nameSurname <$> fullName
  anyChar `manyTill` (try $ lookAhead partyP)
  pParty <- partyP
  anyChar `manyTill` (try $ lookAhead imageLinkP)
  pImageLink <- imageLinkP
  pDescription <- anyChar `manyTill` (string "</p>")
  return $ Profile pName pSurname pParty pImageLink pDescription

--

-- name example :
-- <td class="pagetitle" align="left"><nobr>
--        <span id="_ffffd5f1c06afe12_Main_ctl02_ctl00_ArticleHeading">IRENA PETRIJEVČANIN VUKSANOVIĆ</span>
--        </nobr></td>

fullNameTest :: String
fullNameTest = "<td class=\"pagetitle\" align=\"left\"><nobr>\n        <span id=\"_ffffd5f1c06afe12_Main_ctl02_ctl00_ArticleHeading\">IRENA PETRIJEVČANIN VUKSANOVIĆ</span>\n        </nobr></td>"

fullName :: Parser String
fullName = do
  string "<td class=\"pagetitle\" align=\"left\"><nobr>"
  spaces
  string "<span id=\""
  anyChar `manyTill` (char '>')
  result <- anyChar `manyTill` (char '<')
  return result

--

-- party example :
-- <td class="Stranka" align="right"><nobr>
--              <span>HRID</span></nobr> </td>

partyPTest :: String
partyPTest = "<td class=\"Stranka\" align=\"right\"><nobr>\n              <span>HRID</span></nobr> </td>"

partyP :: Parser String
partyP = do
  string "<td class=\"Stranka\" align=\"right\">"
  anyChar `manyTill` (try $ string "<span>")
  result <- anyChar `manyTill` (string "</span>")
  return result

--

-- image example :
-- <img style="HEIGHT: 219px; WIDTH: 170px" border="0" hspace="5" alt="Irena Petrijevčanin Vuksanović" src="lgs.axd?t=16&amp;id=44476" width="170" align="left" height="219" />

imageLinkPTest :: String
imageLinkPTest = "<img style=\"HEIGHT: 219px; WIDTH: 170px\" border=\"0\" hspace=\"5\" alt=\"Irena Petrijevčanin Vuksanović\" src=\"lgs.axd?t=16&amp;id=44476\" width=\"170\" align=\"left\" height=\"219\" />"

imageLinkP :: Parser String
imageLinkP = do
  string "<img style=\"HEIGHT: 219px; WIDTH: 170px\""
  anyChar `manyTill` (try $ string "src=\"")
  result <- anyChar `manyTill` (char '"')
  anyChar `manyTill` char '>'
  return result



nameSurname :: String -> (String, String)
nameSurname =  separate . map dashing . words . map toLower
  where
    dashing = intercalate "-" . map capitalise . splitOn "-"
    capitalise (s:ss) = toUpper s : ss
    separate (s:ss) = (s, unwords ss)