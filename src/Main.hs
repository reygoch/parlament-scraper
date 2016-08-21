module Main where

import GetHtml
import Parsers

saziv :: String
saziv = "http://www.sabor.hr/zastupnici"

main :: IO ()
main = do
  html <- btos <$> getHtml saziv
  let links = parseLinks saziv html
  (putStrLn . show) $ parseLinks saziv html
  return ()