import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Vector (Vector, freeze, fromList, thaw, toList)
import qualified Data.Vector.Mutable as MV

convertToIntVector :: String -> Vector Int
convertToIntVector = fromList . map read . words


heapify :: Ord a => Vector a -> Int -> Vector a
heapify vector n =
    let getIndexOfParentChildrenMaximum array parentIx n = do
            let leftChildIx = 2 * parentIx + 1
            let rightChildIx = leftChildIx + 1
            maxIx <- if leftChildIx < n then do
                        x <- MV.read array leftChildIx
                        y <- MV.read array parentIx
                        return $ if x > y then leftChildIx else parentIx
                    else return parentIx
            if rightChildIx < n then do
                x <- MV.read array rightChildIx
                y <- MV.read array maxIx
                return $ if x > y then rightChildIx else maxIx
            else return maxIx

        siftDown array parentIx n =
            let go currentParentIx = do
                    maxIx <- getIndexOfParentChildrenMaximum array currentParentIx n
                    when (maxIx /= currentParentIx) $ do
                        MV.swap array maxIx currentParentIx
                        go maxIx
            in go parentIx

        createHeap array parentIx
            | parentIx >= 0 = do
                siftDown array parentIx n
                createHeap array (parentIx - 1)
            | otherwise = return ()
    in runST $ do
        array <- thaw vector
        createHeap array (n `div` 2 - 1)
        freeze array


main :: IO ()
main = do
    n <- readLn
    vector <- convertToIntVector <$> getLine
    let result = heapify vector n
    putStrLn $ unwords $ map show $ toList result
