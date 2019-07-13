module BayesBookmarks where

import qualified Data.Vector as V
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M

-- read line in file
-- split line for words
-- head is class
-- tail is all words that should be added to class

data Tst = Tst (Map String Double)
data Tag = Tag (Map String Tst)

makeTag = undefined

train :: [String] -> [Tags]
train (x:xs)
    | xs == [] = []
    | otherwise = makeTag (take num xs) ++ train (drop num xs)
    where
        t = words $ head lns
        cl = head t
        num = read (tail t) :: Int

main :: IO ()
main = readFile "C:\\Users\\Asus\\Desktop\\dataset.txt" >>= \t -> putStrLn $ head $ lines t
    