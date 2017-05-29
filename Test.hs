module Test where

--functions
sgn x
    | x >= 0 = 1
    | x < 0 = -1

fac x
    | x > 0 = x * fac (x - 1)
    | x == 0 = 1

primes = filterPrimes[2..100] where filterPrimes (p:xs) = p : filterPrimes [x | x <- xs, x `mod` p /= 0]

--custom datatypes
data Color = Red | Green | Blue

say :: Color -> String
say Red   = "You are Red!"
say Blue  = "You are Blue!"
say Green = "You are Green!"

--input
main :: IO ()
main = putStrLn $ "Hello, sky! " ++ (say Blue) 