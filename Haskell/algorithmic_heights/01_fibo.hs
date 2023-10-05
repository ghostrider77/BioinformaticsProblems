fibonacci :: Int -> Int
fibonacci n = fst $ foldl (\(a, b) _ -> (b, a + b)) (0, 1) [1..n]


main :: IO ()
main = do
    n <- readLn
    print $ fibonacci n
