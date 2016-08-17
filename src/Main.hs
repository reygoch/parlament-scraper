module Main where

import GetHtml
import ParseLinks
import Text.Parsec (parse)

saziv :: String
saziv = "http://www.sabor.hr/zastupnici"

main :: IO ()
main = do
  html <- getHtml saziv
  case (parse cols saziv $ btos html) of
    Left _ -> putStrLn "nothing here"
    Right links -> putStrLn $ show $ length links
  --putStrLn $ (show . (parse cols saziv)) $ btos html