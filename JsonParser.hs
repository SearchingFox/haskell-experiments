{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module JsonParser where

import Data.Aeson
import Data.Aeson.TH
import Data.Map                                      (Map, toList)
import Data.Maybe                                    (fromJust)
import GHC.IO.Encoding
import qualified Data.ByteString.Lazy.Char8 as BL

data WebPage = WebPage { url   :: String
                       , title :: String } deriving Show
deriveJSON defaultOptions ''WebPage

newtype All = All{windows :: Map String (Map String WebPage)} deriving Show
deriveJSON defaultOptions ''All

main :: IO ()
main = do
    --setLocaleEncoding utf8 -- or "chcp.com 65001" in console
    f <- BL.readFile "C:\\Users\\Ant\\Desktop\\Sessions - 10-08-2018 06-05-44.json"
    writeFile "C:\\Users\\Ant\\Desktop\\Sessionss.txt" (unlines $ map (\l -> url l ++ " " ++ title l) $ concatMap (map snd . toList . snd) $ concatMap (toList . windows) $ fromJust (decode f :: Maybe [All]))
