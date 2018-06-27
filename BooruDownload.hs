-- Download images from booru sites (danbooru.donmai.us, yande.re, etc.)
-- TODO: add https://konachan.com/
-- TODO: add working with pages
-- TODO: add saving to specific directory
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

splitOnChar :: Char -> String -> [String]
splitOnChar c s = case dropWhile (== c) s of
                "" -> []
                s' -> w : splitOnChar c s''
                    where (w, s'') = break (== c) s'

savePicture :: BS.ByteString -> IO ()
savePicture picUrl = req GET (fst $ fromJust $ parseUrlHttps picUrl) NoReqBody bsResponse mempty >>= \pic ->
    BS.writeFile desktopDir $ responseBody pic
        where fileName = splitOnChar '.' $ last $ splitOnChar '/' $ BS.unpack picUrl
              desktopDir = "C:\\Users\\Ant\\Desktop\\" ++ concat (init fileName) ++ "." ++ last fileName

-- getProperty :: [Tag BS.ByteString] -> [(BS.ByteString, BS.ByteString)]
--                                              id           property

getFilesUrlY :: [Tag BS.ByteString] -> [BS.ByteString]
getFilesUrlY (x:xs) = case x of
    TagClose "posts"        -> []
    TagOpen "post" attrList -> snd (head $ filter (\l -> fst l == "file_url") attrList) : getFilesUrlY xs
    _                       -> getFilesUrlY xs

getFilesUrlD :: [Tag BS.ByteString] -> [BS.ByteString]
getFilesUrlD (x:xs) = case x of -- why (TagOpen x _:y:xs) does not work???
    TagClose "posts"           -> []
    TagOpen "large-file-url" _ -> fromTagText (head xs) : getFilesUrlD xs
    _                          -> getFilesUrlD xs

-- maybe wrap in Maybe
urlToXmlUrlY :: BS.ByteString -> BS.ByteString
urlToXmlUrlY url
    | BS.isInfixOf ".xml?" url = url
    | BS.isInfixOf "show" url  = BS.pack "https://yande.re/post.xml?tags=id:" <> BS.pack (splitOnChar '/' (BS.unpack url) !! 4)
    | BS.isInfixOf "post?" url = BS.pack "https://yande.re/post.xml?" <> BS.pack (last (splitOnChar '?' (BS.unpack url)))
    | otherwise                = url

urlToXmlUrlD :: BS.ByteString -> BS.ByteString
urlToXmlUrlD url
    | BS.isInfixOf ".xml" url   = url
    | BS.isInfixOf "posts/" url = BS.init url <> BS.pack ".xml"
    | BS.isInfixOf "posts?" url = BS.pack "https://danbooru.donmai.us/posts.xml?" <> BS.pack (last (splitOnChar '?' (BS.unpack url)))
    | otherwise                 = url

main :: IO ()
main = do
    mode  <- BS.getLine
    input <- BS.getLine
    let (url, options) = fromJust $ parseUrlHttps $ if BS.isInfixOf "yande.re" input
        then urlToXmlUrlY input
        else urlToXmlUrlD input
    req GET url NoReqBody bsResponse options >>= \r -> mapM_ savePicture $ if BS.isInfixOf "yande.re" input
        then getFilesUrlY $ parseTags $ responseBody r
        else getFilesUrlD $ parseTags $ responseBody r
    
    putStrLn "Done!"
