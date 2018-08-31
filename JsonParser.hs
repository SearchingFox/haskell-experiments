{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module JsonParser where

import Data.Aeson
import Data.Aeson.TH
import Data.Map                                      (Map, toList)
import Data.Maybe                                    (fromJust)
import GHC.IO.Encoding
import System.Directory
import qualified Data.ByteString.Lazy.Char8 as BL

data WebPage = WebPage { url   :: String
                       , title :: String } deriving Show
deriveJSON defaultOptions ''WebPage

newtype All = All { windows :: Map String (Map String WebPage) } deriving Show
deriveJSON defaultOptions ''All

main :: IO ()
main = do
    setLocaleEncoding utf8 -- or "chcp.com 65001" in console
    homeDir <- getHomeDirectory
    file <- BL.readFile $ homeDir ++ "\\Desktop\\Sessions - 29-08-2018 20-13-36.json"
    writeFile (homeDir ++ "\\Desktop\\Sessions.txt") (unlines $ map (\l -> url l ++ " " ++ title l) $ concatMap (map snd . toList . snd) $ concatMap (toList . windows) $ fromJust (decode file :: Maybe [All]))
