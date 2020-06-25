-- Download the latest XKCD comics.
{-# LANGUAGE OverloadedStrings #-}
module Xkcd where

import Network.HTTP.Req
import System.Directory
import Data.Maybe                               (fromJust)
import Control.Exception                        (throwIO)
import qualified Data.ByteString.Char8 as BS

instance MonadHttp IO where
    handleHttpException = throwIO

findImageLink :: [BS.ByteString] -> BS.ByteString
findImageLink pageHtml = last $ BS.words $ head $ filter (BS.isPrefixOf "Image URL") pageHtml

savePicture :: BS.ByteString -> IO ()
savePicture picUrl = getHomeDirectory >>= \homeDir ->
    req GET (fst $ fromJust $ parseUrlHttps picUrl) NoReqBody bsResponse mempty >>= \pic ->
        BS.writeFile (homeDir ++ "\\Desktop\\last_xkcd.png") $ responseBody pic
            

main :: IO ()
main = do
    req GET (https "xkcd.com") NoReqBody bsResponse mempty >>= \bs ->
        savePicture $ findImageLink $ BS.lines $ responseBody bs
    putStrLn "Job's done"
