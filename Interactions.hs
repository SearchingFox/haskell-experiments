import Data.List.Split

--main :: IO ()
-- main = do
--     color <- getLine
--     putStrLn $ "Hello, sky! " ++ (say (conv color))

main :: IO ()
-- main = interact $ unlines . solve . (map (read :: String -> Int)) . tail . lines
main = interact $ unlines . (map con) . (splitOn " ")

cube :: Int -> Int
cube 1 = 1
cube x = x * x *x

con x = x ++ "-"

solve xs = map show ans
    where ans = map cube xs
