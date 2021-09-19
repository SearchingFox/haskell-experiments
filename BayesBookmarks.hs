module BayesBookmarks where

import Data.List ( group, sort )
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

preTrain :: [String] -> [(Cls, [(Wrd, Int)])]
preTrain []     = []
preTrain (x:xs) = (clsName, countSameWords $
    concatMap (words . map (\l -> if l `elem` "@,.?!-:;\"\'()/" then ' ' else l)) a) : preTrain b
    where
        countSameWords = map (\l -> (head l, length l)) . group . sort
        header  = words x
        clsName = head header
        (a, b)  = splitAt (read $ last header) xs

main :: IO ()
main = readFile "dataset1.txt" >>=
    \t -> print $ preTrain $ lines t
