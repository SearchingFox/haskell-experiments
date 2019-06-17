module BayesBookmarks where

import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as M

-- read line in file
-- split line for words
-- head is class
-- tail is all words that should be added to class

data Tst = Tst (Map String Double)
data Tags = Tags (Map String Tst)

train :: [String] -> Tags
train = 
    where
        cl = head x
        num = read y :: Int

main :: IO ()
main = readFile "C:\\Users\\Asus\\Desktop\\dataset.txt" >>= \t -> putStrLn $ head $ lines t
    