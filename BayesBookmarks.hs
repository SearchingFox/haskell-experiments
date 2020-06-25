module BayesBookmarks where

import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
-- import Data.Char
-- import Control.Monad
-- import qualified Data.Vector as V

-- TODO: filter popular words

type Wrd = String  -- ? ByteString
type Cls = String
type Prb = Double
type Tmp = Map Cls Prb
type Tag = Map Wrd Tmp  -- ? data

countSameWords :: [String] -> [(String, Int)]
countSameWords = map (\l -> (head l, length l)) . group . sort

preTrain :: [Wrd] -> [(Cls, [(Wrd, Int)])]
preTrain [] = []
preTrain (x:xs) = (cls_name, countSameWords $ concatMap (words . filter (`notElem` ",.?!-:;\"\'()")) $ take lns_num xs)
    : preTrain (drop lns_num xs)
    where
        header   = words x
        cls_name = head header
        lns_num  = read (last header) :: Int

main :: IO ()
main = readFile "dataset1.txt" >>=
    \t -> print $ preTrain $ lines t
