import Control.Monad (mapM_, replicateM)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.Maybe (listToMaybe, mapMaybe)


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


solve2Sum :: [Int] -> Int -> Int -> Maybe (Int, Int, Int)
solve2Sum xs target targetIx =
    let go negativeTargetIndices [] = Nothing
        go negativeTargetIndices ((item, ix) : rest) =
            case M.lookup item negativeTargetIndices of
                Nothing -> go (M.insert (target - item) ix negativeTargetIndices) rest
                Just jy -> Just (targetIx + 1, jy + 1, ix + 1)
    in go M.empty $ drop (targetIx + 1) $ zip xs [0..]


findZeroSumIndexTriples :: [[Int]] -> [Maybe (Int, Int, Int)]
findZeroSumIndexTriples lists =
    let solve xs = listToMaybe $ mapMaybe (\(item, ix) -> solve2Sum xs (-item) ix) $ zip xs [0..]
    in map solve lists


resultToString :: Maybe (Int, Int, Int) -> String
resultToString Nothing = "-1"
resultToString (Just (i, j, k)) = show i ++ " " ++ show j ++ " " ++ show k


main :: IO ()
main = do
    (k, _) <- readParameters <$> getLine
    lists <- readLists k
    let result = findZeroSumIndexTriples lists
    mapM_ (putStrLn . resultToString) result
