-- Download images from booru sites (danbooru.donmai.us, yande.re, etc.)
-- TODO: add https://konachan.com/
-- TODO: add working with pages
-- TODO: add saving to specific directory
-- TODO: maybe add cli
{-# LANGUAGE OverloadedStrings #-}
module Booru where

import Control.Monad
import Network.HTTP.Req
import Text.HTML.TagSoup
import Data.Maybe                               (fromJust)
import Control.Exception                        (throwIO)
import qualified Data.ByteString.Char8 as BS

instance MonadHttp IO where
    handleHttpException = throwIO

splitOnChar :: Char -> String -> [String]
splitOnChar splitOnCharc s = case dropWhile (== c) s of
                "" -> []
                s' -> w : splitOnChar c s''
                    where (w, s'') = break (== c) s'

savePicture :: BS.ByteString -> IO ()
savePicture picUrl = req GET (fst $ fromJust $ parseUrlHttps picUrl) NoReqBody bsResponse mempty >>= \pic ->
    BS.writeFile ("C:\\Users\\Ant\\Desktop\\" ++ head (tail fileName) ++ "." ++ last fileName) $ responseBody pic
        where fileName = splitOnChar '.' $ last $ splitOnChar '/' $ BS.unpack picUrl

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


main :: IO ()
main = do
    let Just (url, options) = parseUrlHttps "https://yande.re/post.xml?tags=liang_xing&page=1"
    req GET url NoReqBody bsResponse options >>= \r ->
        mapM savePicture $ getFilesUrlY $ parseTags $ responseBody r
    putStrLn "Done!"
