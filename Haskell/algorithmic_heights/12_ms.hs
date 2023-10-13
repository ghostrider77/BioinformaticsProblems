convertToIntList :: String -> [Int]
convertToIntList = map read . words


mergeSortedLists :: Ord a => [a] -> [a] -> [a]
mergeSortedLists [] [] = []
mergeSortedLists xs [] = xs
mergeSortedLists [] ys = ys
mergeSortedLists (x : xs) (y : ys) =
    if x <= y then x : mergeSortedLists xs (y : ys)
    else y : mergeSortedLists (x : xs) ys


mergeSort :: Ord a => [a] -> Int -> [a]
mergeSort xs n
    | n <= 1 = xs
    | otherwise =
        let k = n `div` 2
            (xs1, xs2) = splitAt k xs
            sorted1 = mergeSort xs1 k
            sorted2 = mergeSort xs2 (n - k)
        in mergeSortedLists sorted1 sorted2


main :: IO ()
main = do
    n <- readLn
    xs <- convertToIntList <$> getLine
    let result = mergeSort xs n
    putStrLn $ unwords $ map show result
