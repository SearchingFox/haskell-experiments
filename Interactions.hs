import Data.List.Split

-- main :: IO ()
-- main = do
--     color <- getLine
--     putStrLn $ "Hello, sky! You're " ++ color

main :: IO ()
main = interact $ unlines . (map con) . (splitOn " ")
-- main = interact $ unlines . solve . (map (read :: String -> Int)) . tail . lines

con x = x ++ "-"

cube :: Int -> Int
cube 1 = 1
cube x = x * x *x

solve xs = map show ans
    where ans = map cube xs
