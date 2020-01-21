module Test where
import Data.Bits
import Prelude hiding (getContents,putStr)
import System.IO.Encoding

-- custom types
data Time   = Time Hour Minute Second
data Hour   = Hour   Int
data Minute = Minute Int
data Second = Second Int

data Color = Red | Green | Blue


-- functions
sgn a
    | a < 0  = -1
    | a >= 0 = 1

fac x
    | x > 0  = x * fac (x - 1)
    | x == 0 = 1

primes n = filterPrimes [2..n] where
    filterPrimes []     = []
    filterPrimes (p:xs) = p : filterPrimes [x | x <- xs, mod x p /= 0]

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ ini [] = [ini]
scanr f ini (x:xs) =


qsort :: (Ord a) => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort [n | n <- xs, n <= x] ++ [x] ++ qsort [n | n <- xs, n > x]

pow :: Integer -> Integer -> Integer
pow _ 0 = 1
pow n m
    | even m    = pow (n * n) (div m 2)
    | otherwise = n * pow n (m - 1)

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

cubesforlist y = map (\x -> pow x 3) y

split :: [a] -> [ ([a], [a]) ]
split []     = [ ([], []) ]
split (x:xs) = ([], x:xs) : [(x:bs, cs) | (bs, cs) <- split xs]

permutations :: [a] -> [[a]]
permutations []     = [[]]
permutations (x:xs) = [bs ++ x:cs | perm <- permutations xs, (bs, cs) <- split perm]

evenDec :: Integral a => [a] -> [a]
evenDec x = [if y `mod` 2 == 0 then y - 1 else y | y <- x]
-- evenDec [] = []
-- even_dec (x:xs) = [x + (x `mod` 2 - 1)] ++ even_dec xs

digits :: Int -> [Int] -> [Int]
digits 0 res   = reverse res
digits num res = digits (num `div` 10) (res ++ [num `mod` 10])

contNum :: (Num t, Eq t) => t -> Int -> Int
contNum 0 num = num
contNum n num = contNum (n-1) (num `div` (10^(len-1)) * 10 ^ len + num) where
    len = length (digits num [])

say :: Color -> String
say Red   = "You are Red!"
say Blue  = "You are Blue!"
say Green = "You are Green!"

conv :: String -> Color
conv "Blue"  = Blue
conv "Green" = Blue
conv "Red"   = Blue

main = do
  e <- getSystemEncoding
  let enc = e
  str <- getContents
  putStr str

-- bor :: (Num a) => a -> a -> Bool
-- bor a b = a  b


-- binary tree
data Tree a = Nil | Branch a (Tree a) (Tree a)
    deriving (Ord, Eq, Show)

insert :: (Ord a) => Tree a -> a -> Tree a
insert Nil x = Branch x Nil Nil
insert (Branch num left right) x
    | num == x = Branch num left right
    | num < x  = Branch num left (insert right x)
    | num > x  = Branch num (insert left x) right

find :: (Ord a) => Tree a -> a -> Bool
find Nil _ = False
find (Branch num left right) x
    | num == x = True
    | num < x  = find right x
    | num > x  = find left x

inorder :: (Ord a) => Tree a -> [a]
inorder Nil = []
inorder (Branch num left right) = inorder left ++ [num] ++ inorder right
