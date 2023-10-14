import Control.Monad (foldM)
import Control.Monad.ST (runST)
import Data.Vector (Vector, fromList, thaw)
import qualified Data.Vector.Mutable as MV


convertToIntVector :: String -> Vector Int
convertToIntVector = fromList . map read . words


calcInsertionSortSwaps :: Vector Int -> Int -> Int
calcInsertionSortSwaps vector n =
    let go array nrSwaps k = do
            if k == 0 then return nrSwaps
            else do
                x <- MV.read array k
                y <- MV.read array (k - 1)
                if x >= y then return nrSwaps
                else do
                    MV.swap array k (k - 1)
                    go array (nrSwaps + 1) (k - 1)
    in runST $ do
        array <- thaw vector
        foldM (go array) 0 [1..(n-1)]


main :: IO ()
main = do
    n <- readLn
    vector <- convertToIntVector <$> getLine
    let result = calcInsertionSortSwaps vector n
    print result
