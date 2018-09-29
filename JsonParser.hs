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
                       , title :: Maybe String } deriving Show
deriveJSON defaultOptions ''WebPage

newtype All = All { windows :: Map String (Map String WebPage) } deriving Show
deriveJSON defaultOptions ''All

main :: IO ()
main = do
    setLocaleEncoding utf8 -- or "chcp.com 65001" in console
    homeDir <- getHomeDirectory
    file    <- BL.readFile $ homeDir ++ "\\Desktop\\Сессии - 2018-09-26 22-52-00.json"

    writeFile (homeDir ++ "\\Desktop\\FirefoxSavedTabsExport.txt") $ unlines $
        map (\l -> case title l of
                Nothing -> url l ++ "\n" ++ "None"
                _ -> url l ++ "\n" ++ fromJust (title l) ) $
                    concatMap (map snd . toList . snd) $
                    concatMap (toList . windows) $ fromJust (decode file :: Maybe [All])
