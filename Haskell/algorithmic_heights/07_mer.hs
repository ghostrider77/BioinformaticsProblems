convertToIntList :: String -> [Int]
convertToIntList = map read . words


mergeSortedLists :: Ord a => [a] -> [a] -> [a]
mergeSortedLists [] [] = []
mergeSortedLists xs [] = xs
mergeSortedLists [] ys = ys
mergeSortedLists (x : xs) (y : ys) =
    if x <= y then x : mergeSortedLists xs (y : ys)
    else y : mergeSortedLists (x : xs) ys


main :: IO ()
main = do
    _ <- readLn :: IO Int
    xs <- convertToIntList <$> getLine
    _ <- readLn :: IO Int
    ys <- convertToIntList <$> getLine
    let result = mergeSortedLists xs ys
    putStrLn $ unwords $ map show result
