module BayesBookmarks where

import Data.Char
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Vector as V

-- read line in file
-- split line for words
-- head is class
-- tail is all words that should be added to class

type Wrd  = String
type Class = String
type Probability = Double
data Tst = Tst (Map Class Probability)  -- ? type
data Tag = Tag (Map Wrd Tst)

-- filter popular words
f :: String -> String
f s@(c:cs)
    | cs == [] = []
    | c == ' ' = ' ' : f cs
    | isLetter c = toLower c : f cs
    | otherwise = ' ' : f cs
--filter (not . (`elem` ",.?!-:;\"\'"))
makeTag :: [String] -> [(String, Int)]
makeTag [] = []
makeTag (x:xs) = (f x, 0) : makeTag xs

-- ? ByteString
train :: [String] -> [(String, [(String, Int)])]
train [] = []
train (x:xs) = (cl, makeTag (take num xs)) : train (drop num xs)
    where
        t  = words x
        cl = head t
        num = read (last t) :: Int

main :: IO ()
main = readFile "C:\\Users\\Asus\\Desktop\\dataset1.txt" >>= \t -> print $ train $ lines t
    