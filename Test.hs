module Test where

sgn x
    | x >= 0 = 1
    | x < 0 = -1

fac x
    | x > 0 = x * fac (x - 1)
    | x == 0 = 1

primes = filterPrimes[2..100] where filterPrimes (p:xs) = p : filterPrimes [x | x <- xs, x `mod` p /= 0]