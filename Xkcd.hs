--{-# LANGUAGE OverloadedStrings #-}
module Xkcd where

import Data.Char
import Network.HTTP
import Text.Html.DOM
import Text.XML.Cursor

url = "http://xkcd.com"

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

haskellLastModifiedDateTime :: IO ()
haskellLastModifiedDateTime = do
    src <- openURL url
    let lastModifiedDateTime = fromFooter $ parseTags src
    putStrLn $ lastModifiedDateTime ++ "Hey"
    --where fromFooter = unwords . drop 6 . words . innerText . take 2 . dropWhile (~/= "<div id=comic>")
    where fromFooter = unwords . words . innerText . take 3 . dropWhile (~/= "<div id=comic>")
    

main :: IO ()
main = haskellLastModifiedDateTime
