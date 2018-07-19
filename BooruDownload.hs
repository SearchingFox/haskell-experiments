-- Download images from booru sites (danbooru.donmai.us, yande.re, etc.)
-- TODO: add https://konachan.com/
-- TODO: add working with pages
-- TODO: maybe add cli
{-# LANGUAGE OverloadedStrings #-}
module Booru (getFilesUrlY, getFilesUrlD) where

import Network.HTTP.Req
import Text.HTML.TagSoup
import System.Environment
import System.Directory
import Data.Maybe                               (fromJust)
import Control.Monad                            (mapM)
import Control.Exception                        (throwIO)
import qualified Data.ByteString.Char8 as BS

instance MonadHttp IO where
    handleHttpException = throwIO

savePicture :: String -> BS.ByteString -> IO ()
savePicture dir picUrl =
    req GET (fst $ fromJust $ parseUrlHttps picUrl) NoReqBody bsResponse mempty >>= \pic ->
        BS.writeFile filePath $ responseBody pic where
            fileName = map BS.unpack $ BS.split '.' $ last $ BS.split '/' picUrl
            filePath = dir ++ "\\" ++  concat (init fileName) ++ "." ++ last fileName

savePictures :: String -> [BS.ByteString] -> IO ()
savePictures dir lst = do
    homeDir <- getHomeDirectory
    createDirectoryIfMissing False (homeDir ++ "\\Desktop\\" ++ dir)
    mapM_ (savePicture (homeDir ++ "\\Desktop\\" ++ dir)) lst

-- getProperty :: [Tag BS.ByteString] -> [(BS.ByteString, BS.ByteString)]
--                                              id           property

getFilesUrlY :: [Tag BS.ByteString] -> [BS.ByteString]
getFilesUrlY (x:xs) = case x of
    TagClose "posts"        -> []
    TagOpen "post" attrList -> snd (head $ filter (\l -> fst l == "file_url") attrList) : getFilesUrlY xs
    _                       -> getFilesUrlY xs

getFilesUrlD :: [Tag BS.ByteString] -> [BS.ByteString]
getFilesUrlD (x:xs) = case x of
    TagClose "posts"           -> []
    TagOpen "large-file-url" _ -> fromTagText (head xs) : getFilesUrlD xs
    _                          -> getFilesUrlD xs

getPoolD :: [Tag BS.ByteString] -> [BS.ByteString]
getPoolD (x:xs) = case x of
    TagOpen "post-ids" _       -> BS.words (fromTagText (head xs)) -- : getFilesUrlD xs
    _                          -> getFilesUrlD xs

-- maybe wrap in Maybe
urlToXmlUrlY :: BS.ByteString -> BS.ByteString
urlToXmlUrlY url
    | BS.isInfixOf ".xml?" url = url
    | BS.isInfixOf "post?" url = BS.pack "https://yande.re/post.xml?"         <> last (BS.split '?' url)
    | BS.isInfixOf "pool"  url = BS.pack "https://yande.re/pool/show.xml?id=" <> last (BS.split '/' url)
    | BS.isInfixOf "show"  url = BS.pack "https://yande.re/post.xml?tags=id:" <> BS.split '/' url !! 4
    | otherwise                = url -- error

urlToXmlUrlD :: BS.ByteString -> BS.ByteString
urlToXmlUrlD url
    | BS.isInfixOf ".xml"   url = url
    | BS.isInfixOf "posts/" url = BS.init url <> BS.pack ".xml"
    | BS.isInfixOf "pools"  url = BS.init url <> BS.pack ".xml"
    | BS.isInfixOf "posts?" url = BS.pack "https://danbooru.donmai.us/posts.xml?" <> last (BS.split '?' url)
    | otherwise                 = url

main :: IO ()
main = do
    dir   <- getLine
    input <- BS.getLine
    let (url, options) = fromJust $ parseUrlHttps $ (if BS.isInfixOf "yande.re" input
        then urlToXmlUrlY else urlToXmlUrlD) input
    req GET url NoReqBody bsResponse options >>= \r -> savePictures dir $ (if BS.isInfixOf "yande.re" input
        then getFilesUrlY else getFilesUrlD) $ parseTags $ responseBody r
    --req GET url NoReqBody bsResponse options >>= \r -> print $ getPoolD $ parseTags $ responseBody r
    
    putStrLn "Done!"
