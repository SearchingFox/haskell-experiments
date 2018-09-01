-- Download images from booru sites (danbooru.donmai.us, yande.re, etc.)
-- TODO: add konachan, zerochan (?), derpibooru, gelbooru
-- TODO: add working with pages
-- TODO: maybe add cli
{-# LANGUAGE OverloadedStrings #-}
module BooruDownload (main) where

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
savePicture dir picUrl = do
    let filePath = dir ++ "\\" ++ replace "%20" "_" fileName where
            fileName = BS.unpack $ last $ BS.split '/' picUrl
            replace :: String -> String -> String -> String
            replace old new s@(x:xs)
                | take (length old) s == old = new ++ replace old new (drop (length old) s)
                | otherwise = x : replace old new xs
            replace _ _ "" = []
    req GET (fst $ fromJust $ parseUrlHttps picUrl) NoReqBody bsResponse mempty >>= \pic ->
        BS.writeFile filePath $ responseBody pic
    
    putStrLn $ "Downloaded " ++ filePath

savePictures :: String -> [BS.ByteString] -> IO ()
savePictures dir lst = do
    homeDir <- getHomeDirectory
    createDirectoryIfMissing False (homeDir ++ "\\Desktop\\" ++ dir)
    mapM_ (savePicture (homeDir ++ "\\Desktop\\" ++ dir)) lst


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

-- TODO: add Nothing handling
downloadLink :: BS.ByteString -> IO ()
downloadLink input = do
    let dirr = BS.unpack $ last $ BS.split '/' input
    let (url, options) = fromJust $ parseUrlHttps $ (if BS.isInfixOf "yande.re" input
        then urlToXmlUrlY else urlToXmlUrlD) input

    putStrLn $ "Downloading " ++ BS.unpack input

    req GET url NoReqBody bsResponse options >>= \rsp -> savePictures dirr $ (if BS.isInfixOf "yande.re" input
        then getFilesUrlY else getFilesUrlD) $ parseTags $ responseBody rsp

downloadFromFile :: String -> IO ()
downloadFromFile file = readFile file >>= \strings ->
    mapM_ (downloadLink . BS.pack) $ lines strings

main :: IO ()
main = do
    getArgs >>= \args -> case args of
        ["-f", file] -> downloadFromFile file
        [_] -> mapM_ (downloadLink . BS.pack) args
        [] -> do
            putStrLn "Enter URL:"
            getLine >>= downloadLink . BS.pack

    putStrLn "Done!"
