module Test where

--functions
sgn x
    | x >= 0 = 1
    | x < 0 = -1

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
pow n 0 = 1
pow n m
    | even m = pow (n * n) (div m 2)
    | otherwise = n * pow n (m - 1)

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

cubesforlist y = map (\x -> x * x * x) y

--conditional exp
haskell = if 1==1 then "awesome" else "awful"


--custom datatypes
data Color = Red | Green | Blue

say :: Color -> String
say Red   = "You are Red!"
say Blue  = "You are Blue!"
say Green = "You are Green!"


data Maybe a = Nothing | Just a

--input
main :: IO ()
main = putStrLn $ "Hello, sky! " ++ (say Blue) 