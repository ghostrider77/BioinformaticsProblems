convertToIntList :: String -> [Int]
convertToIntList = map read . words


mergeSortedLists :: Ord a => [a] -> Int -> [a] -> Int -> ([a], Int)
mergeSortedLists lst1 n1 lst2 inversions =
    let go acc xs len ys inv =
            case (xs, ys) of
                ([], []) -> (reverse acc, inv)
                (x : xss, []) -> go (x : acc) xss (len - 1) ys inv
                ([], y : yss) -> go (y : acc) xs len yss inv
                (x : xss, y : yss) ->
                    if x <= y then go (x : acc) xss (len - 1) ys inv
                    else go (y : acc) xs len yss (inv + len)
    in go [] lst1 n1 lst2 inversions


countInversions :: Ord a => [a] -> Int -> ([a], Int)
countInversions xs n
    | n <= 1 = (xs, 0)
    | otherwise =
        let k = n `div` 2
            (xs1, xs2) = splitAt k xs
            (sorted1, inv1) = countInversions xs1 k
            (sorted2, inv2) = countInversions xs2 (n - k)
        in mergeSortedLists sorted1 k sorted2 (inv1 + inv2)


main :: IO ()
main = do
    n <- readLn
    xs <- convertToIntList <$> getLine
    let (_, result) = countInversions xs n
    print result
