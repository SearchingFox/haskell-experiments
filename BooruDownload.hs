{-# LANGUAGE OverloadedStrings #-}
module Booru where

import Network.HTTP.Req
import Text.HTML.TagSoup
import Data.Maybe                               (fromJust)
import Control.Exception                        (throwIO)
import qualified Data.ByteString.Char8 as BS

instance MonadHttp IO where
    handleHttpException = throwIO

savePicture :: BS.ByteString -> BS.ByteString -> IO ()
savePicture ext picUrl = req GET (fst $ fromJust $ parseUrlHttps picUrl) NoReqBody bsResponse mempty >>= \pic ->
    BS.writeFile ("C:\\Users\\Ant\\Desktop\\last_pic." ++ BS.unpack ext) $ responseBody pic

-- getProperty :: [Tag BS.ByteString] -> [(BS.ByteString, BS.ByteString)]
--                                              id           property

getFileUrlY :: [Tag BS.ByteString] -> [BS.ByteString]
getFileUrlY (x:xs) = case x of
    TagClose "posts"        -> []
    TagOpen "post" attrList -> snd (head $ filter (\l -> fst l == "file_url") attrList) : getFileUrlY xs
    _                       -> getFileUrlY xs

getFileUrlD :: [Tag BS.ByteString] -> [BS.ByteString]
getFileUrlD (x:xs) = case x of -- why (TagOpen x _:y:xs) does not work???
    TagClose "posts"           -> []
    TagOpen "large-file-url" _ -> fromTagText (head xs) : getFileUrlD xs
    _                          -> getFileUrlD xs
    

main :: IO ()
main = do
    -- danbooru - large-file-url -tag
    -- yandere - file_url -attribute
    let Just (url, options) = parseUrlHttps "https://yande.re/post.xml?tags=liang_xing&page=1"
    -- let Just (url, options) = parseUrlHttps "https://danbooru.donmai.us/posts.xml?tags=liang_xing&page=1"
    req GET url NoReqBody bsResponse options >>= \r ->
        print $ length $ getFileUrlY $ parseTags $ responseBody r
        -- savePicture "jpg" $ getLastImg
