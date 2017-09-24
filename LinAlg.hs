module LinAlg where

dota :: Num a => [a] -> [a] -> a
dota [] [] = 0
dota (x:xs) (y:ys) = x * y + dot xs ys

dot :: Num a => [a] -> [a] -> a
dot a b
    | length a == 0 = 0
    | length a == length b = head a * head b + dot (tail a) (tail b)

transpose (Ord a) => [a] -> [a]
