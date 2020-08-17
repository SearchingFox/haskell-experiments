-- Download images from booru sites (danbooru.donmai.us, yande.re, etc.)
-- TODO: add konachan, zerochan (?), derpibooru, gelbooru
-- TODO!: add working with pages
{-# LANGUAGE OverloadedStrings #-}
module BooruDownload where

import Network.HTTP.Req
import Text.HTML.TagSoup
import System.Environment
import System.Directory
import Data.Time.Clock.POSIX
import Data.Char
import Data.Maybe                               (fromJust)
import Control.Exception                        (throwIO)
import Control.Monad                            (mapM)
import qualified Data.ByteString.Char8 as BS

instance MonadHttp IO where
    handleHttpException = throwIO

type Danbooru = [Tag BS.ByteString]

-- TODO?: Move IO action to main
savePicture :: String -> BS.ByteString -> IO ()
savePicture dir picUrl = do
    let filePath = dir ++ "\\" ++ fileName where
        fileName = filter (`notElem` ("/\\:*?\"<>|" :: String)) $ unescapeString $ BS.unpack $ last $ BS.split '/' picUrl where
            unescapeString (x:s@(y:z:xs))
                | x == '%'  = chr (read ("0x" ++ [y, z])) : unescapeString xs
                | otherwise = x : unescapeString s
            unescapeString s = s
    
    req GET (fst $ fromJust $ parseUrlHttps picUrl) NoReqBody bsResponse mempty >>= \pic ->
        BS.writeFile filePath $ responseBody pic
    putStrLn $ "Downloaded " ++ filePath

savePictures :: String -> [BS.ByteString] -> IO ()
savePictures dir lst = do
    createDirectoryIfMissing False ("D:\\Downloadss\\" ++ dir)
    mapM_ (savePicture $ "D:\\Downloadss\\" ++ dir) lst

getPoolD :: [Tag BS.ByteString] -> [BS.ByteString]
getPoolD (x:xs) = case x of
    TagOpen "post-ids" _ -> map ("https://danbooru.donmai.us/posts/" <>) $ BS.words $ fromTagText $ head xs
    _                    -> getPoolD xs

getFilesUrlY :: [Tag BS.ByteString] -> [BS.ByteString]
getFilesUrlY (x:xs) = case x of
    TagClose "posts"        -> []
    TagOpen "post" attrList -> snd (head $ filter (\l -> fst l == "file_url") attrList) : getFilesUrlY xs
    _                       -> getFilesUrlY xs

-- getFilesUrlD :: [Tag BS.ByteString] -> [BS.ByteString]
-- getFilesUrlD (x:xs@(y:_)) = case x of
--     TagClose "posts"     -> []
--     TagOpen "file-url" _ -> fromTagText y : getFilesUrlD xs
--     _                    -> getFilesUrlD xs

getFilesUrlD :: [Tag BS.ByteString] -> [BS.ByteString]
getFilesUrlD (x:y:xs) = case x of
    TagClose "posts"     -> []
    TagOpen "file-url" _ -> fromTagText y : getFilesUrlD xs
    _                    -> getFilesUrlD xs

getFilesUrlG :: [Tag BS.ByteString] -> [BS.ByteString]
getFilesUrlG = getFilesUrlY

-- ? maybe wrap in Maybe
urlToXmlUrlY :: BS.ByteString -> BS.ByteString
urlToXmlUrlY url
    | BS.isInfixOf ".xml"  url = url
    | BS.isInfixOf "pool"  url = url <> BS.pack ".xml"
    | BS.isInfixOf "post?" url = BS.pack "https://yande.re/post.xml?"         <> last (BS.split '?' url)
    | BS.isInfixOf "show"  url = BS.pack "https://yande.re/post.xml?tags=id:" <> BS.split '/' url !! 5
    | otherwise                = error "Unsupported Yandere link"

urlToXmlUrlD :: BS.ByteString -> BS.ByteString
urlToXmlUrlD url
    | BS.isInfixOf ".xml"   url = url
    | BS.isInfixOf "pools"  url = url <> BS.pack ".xml"
    | BS.isInfixOf "posts/" url = url <> BS.pack ".xml"
    | BS.isInfixOf "posts?" url = BS.intercalate ".xml?" $ BS.split '?' url
    | otherwise                 = error "Unsupported Danbooru link" -- ? error ?

urlToXmlUrl :: BS.ByteString -> BS.ByteString
urlToXmlUrl url
    | BS.isInfixOf "yande.re"           url = urlToXmlUrlY url
    | BS.isInfixOf "danbooru.donmai.us" url = urlToXmlUrlD url
    | BS.isInfixOf "gelbooru.com"       url = BS.pack "https://gelbooru.com/index.php?page=dapi&s=post&q=index&id=" <> last (BS.split '/' url)
    | BS.isInfixOf "konachan.com"       url = undefined
    | otherwise                             = url

chooseParser :: BS.ByteString -> ([Tag BS.ByteString] -> [BS.ByteString])
chooseParser url
    | BS.isInfixOf "yande.re"  url = getFilesUrlY
    | BS.isInfixOf "donmai.us" url = getFilesUrlD

-- ? remove Nothing handling? Handling empty input
downloadLink :: BS.ByteString -> IO ()
downloadLink link = do
    -- case dirr of
    --     "time" -> do
    --         t <- getPOSIXTime
    --         let dirName = show $ round t
    let dirName = filter (flip notElem ("/\\:*?\"<>|" :: String)) $ BS.unpack $ last $
            BS.split '/' (if BS.last link == '/' then BS.init link else link)
    case parseUrlHttps $ urlToXmlUrl link of
        Just (url, options) -> do
            putStrLn $ "Downloading " ++ BS.unpack link
            req GET url NoReqBody bsResponse options >>= \rsp ->
                savePictures dirName $ chooseParser link $ parseTags $ responseBody rsp
        Nothing -> putStrLn "Bad url"

downloadFromFile :: String -> IO ()
downloadFromFile file = readFile file >>= \s -> mapM_ (downloadLink . BS.pack) $ lines s

main :: IO ()
main = do
    args <- getArgs
    -- let dir = if head args == "-d" then Just (args !! 1) else Nothing
    case args of
        ["-f", file] -> downloadFromFile file
        [_]          -> mapM_ (downloadLink . BS.pack) args
        []           -> do
                        putStrLn "Enter URL:"
                        getLine >>= downloadLink . BS.pack

    putStrLn "Done!"
