module LinAlg where

dot :: Num a => [a] -> [a] -> a
dota [] [] = 0
dota (x:xs) (y:ys) = x * y + dot xs ys

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)
