module Test where

-- type
data Time   = Time Hour Minute Second
data Hour   = Hour   Int
data Minute = Minute Int
data Second = Second Int
    

-- functions
sgn a
    | a < 0 = -1
    | a >= 0 = 1

fac x
    | x > 0 = x * fac (x - 1)
    | x == 0 = 1

primes = filterPrimes [2..100]
    where
        filterPrimes [] = []
        filterPrimes (p:xs) = p : filterPrimes [x | x <- xs, mod x p /= 0]

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [n | n <- xs, n <= x] ++ [x] ++ qsort [n | n <- xs, n > x]

pow :: Integer -> Integer -> Integer
pow _ 0 = 1
pow n m
    | even m = pow (n * n) (div m 2)
    | otherwise = n * pow n (m - 1)

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

cubesforlist y = map (\x -> pow x 3) y

splits :: [a] -> [ ([a], [a]) ]
splits [] = [ ([], []) ]
splits (x:xs) = ([], x:xs) : [(x:bs, cs) | (bs, cs) <- splits xs]

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = [bs ++ x:cs | perm <- perms xs, (bs, cs) <- splits perm]


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
    | num < x = find right x
    | num > x = find left x

inorder :: (Ord a) => Tree a -> [a]
inorder Nil = []
inorder (Branch num left right) = inorder left ++ [num] ++ inorder right

-- custom datatypes
data Color = Red | Green | Blue

say :: Color -> String
say Red   = "You are Red!"
say Blue  = "You are Blue!"
say Green = "You are Green!"

conv :: String -> Color
conv "Blue" = Blue

data Maybe a = Nothing | Just a

--input
-- main :: IO ()
-- main = do
--     color <- getLine
--     putStrLn $ "Hello, sky! " ++ (say (conv color))
