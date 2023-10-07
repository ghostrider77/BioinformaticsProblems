import Control.Monad (mapM_, replicateM)
import Data.IntMap (IntMap)
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


findZeroSumIndexPairs :: [[Int]] -> [Maybe (Int, Int)]
findZeroSumIndexPairs lists =
    let solve xs =
            let go acc ix [] = Nothing
                go acc ix (x : xss) =
                    case M.lookup x acc of
                        Nothing -> go (M.insert (-x) ix acc) (ix + 1) xss
                        Just jy -> Just (jy + 1, ix + 1)
            in go M.empty 0 xs
    in map solve lists


resultToString :: Maybe (Int, Int) -> String
resultToString Nothing = "-1"
resultToString (Just (i, j)) = show i ++ " " ++ show j


main :: IO ()
main = do
    (k, _) <- readParameters <$> getLine
    lists <- readLists k
    let result = findZeroSumIndexPairs lists
    mapM_ (putStrLn . resultToString) result
