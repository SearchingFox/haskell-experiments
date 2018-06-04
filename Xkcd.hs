-- Download the latest XKCD comics.
{-# LANGUAGE OverloadedStrings #-}
module Xkcd where

import Network.HTTP.Req
import Data.Maybe                               (fromJust)
import Control.Exception                        (throwIO)
import qualified Data.Text
import qualified Data.ByteString.Char8 as BS

instance MonadHttp IO where
    handleHttpException = throwIO

findImageLink :: [BS.ByteString] -> BS.ByteString
findImageLink pageHtml = last $ BS.words $ head $ filter (BS.isPrefixOf "Image URL") pageHtml

savePicture :: BS.ByteString -> IO ()
savePicture picUrl = do
    pic <- req GET (fst $ fromJust $ parseUrlHttps picUrl) NoReqBody bsResponse mempty
    BS.writeFile "C:\\Users\\Ant\\Desktop\\last_xkcd.png" $ responseBody pic

main :: IO ()
main = do
    req GET (https "xkcd.com") NoReqBody bsResponse mempty >>= \bs ->
        savePicture $ findImageLink $ BS.lines $ responseBody bs
    putStrLn "Job's done"
