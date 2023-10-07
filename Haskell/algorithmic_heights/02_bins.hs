import Data.Vector (Vector, (!), fromList, last)


convertToIntList :: String -> [Int]
convertToIntList = map read . words


findElemsInSortedArray :: Vector Int -> [Int] -> Int -> [Int]
findElemsInSortedArray array queries n =
    let binarySearch item left right =
            if left > right then -1
            else
                let ix = (left + right) `div` 2
                    middleElem = array ! ix
                in if middleElem == item then ix + 1
                else if middleElem < item then binarySearch item (ix + 1) right
                else binarySearch item left (ix - 1)
    in map (\query -> binarySearch query 0 (n - 1)) queries


main :: IO ()
main = do
    n <- readLn
    _ <- readLn :: IO Int
    array <- fromList . convertToIntList <$> getLine
    queries <- convertToIntList <$> getLine
    let result = findElemsInSortedArray array queries n
    putStrLn $ unwords $ map show result
