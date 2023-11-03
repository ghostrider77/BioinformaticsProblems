{-# LANGUAGE TupleSections #-}

import Control.Monad (replicateM)
import Data.IntMap (IntMap, fromListWith)
import qualified Data.IntMap as M

convertToIntList :: String -> [Int]
convertToIntList = map read . words


readParameters :: String -> (Int, Int)
readParameters line =
    case convertToIntList line of
        [a, b] -> (a, b)
        _ -> error "Malformed input."


readLists :: Int -> IO [[Int]]
readLists k = do
    rows <- replicateM k getLine
    return $ map convertToIntList rows


calcMajorityElements :: [[Int]] -> Int -> [Int]
calcMajorityElements lists n =
    let majorityelement xs =
            let occurrences = fromListWith (+) $ map (, 1) xs
                findMax item cnt (mostCommon, maxCount) =
                    if cnt > maxCount then (item, cnt)
                    else (mostCommon, maxCount)
                (mostCommon, count) = M.foldrWithKey findMax (0, 0) occurrences
            in if count > n `div` 2 then mostCommon else -1
    in map majorityelement lists


main :: IO ()
main = do
    (k, n) <- readParameters <$> getLine
    lists <- readLists k
    let result = calcMajorityElements lists n
    putStrLn $ unwords $ map show result
