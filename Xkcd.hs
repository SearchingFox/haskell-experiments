-- Download the latest XKCD comics.
{-# LANGUAGE OverloadedStrings #-}
module Xkcd where

import Data.Maybe
import Data.Text hiding (head, last, tail, takeWhile)
import Network.HTTP.Req
import Data.Text.Encoding
import Control.Exception (throwIO)
import qualified Data.ByteString.Char8 as BS

instance MonadHttp IO where
    handleHttpException = throwIO

findImageLink :: [BS.ByteString] -> BS.ByteString
findImageLink x
    | BS.isPrefixOf "Image URL" y = last (BS.words y)
    | otherwise                   = findImageLink (tail x)
    where y = head x
findImageLink_1 x = last $ BS.words $ head $ takeWhile (\y -> BS.isPrefixOf "Image URL" y) x

savePicture :: BS.ByteString -> IO ()
savePicture picUrl = do
    pic <- req GET (fst $ fromJust $ parseUrlHttps picUrl) NoReqBody bsResponse mempty
    BS.writeFile "C:\\Users\\Ant\\Desktop\\last_xkcd.png" $ responseBody pic

main :: IO ()
main = do
    req GET (https "xkcd.com") NoReqBody bsResponse mempty >>= \bs ->
        savePicture $ findImageLink_1 $ BS.lines $ responseBody bs
    BS.putStrLn "Job's done"
    -- req GET (https "xkcd.com") NoReqBody bsResponse mempty >>= \bs ->
    --     BS.putStrLn $ findImageLink $ BS.lines $ responseBody bs
