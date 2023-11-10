import Control.Monad (mapM_, when)
import Control.Monad.ST (runST)
import Data.Vector (Vector, freeze, fromList, thaw, toList)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

convertToIntVector :: String -> Vector Int
convertToIntVector = fromList . map read . words


partialSort :: Ord a => Vector a -> Int -> Int -> Vector a
partialSort vector n k =
    let getIndexOfParentChildrenMinimum array parentIx n = do
            let leftChildIx = 2 * parentIx + 1
            let rightChildIx = leftChildIx + 1
            minIx <- if leftChildIx < n then do
                        x <- MV.read array leftChildIx
                        y <- MV.read array parentIx
                        return $ if x < y then leftChildIx else parentIx
                    else return parentIx
            if rightChildIx < n then do
                x <- MV.read array rightChildIx
                y <- MV.read array minIx
                return $ if x < y then rightChildIx else minIx
            else return minIx

        siftDown array parentIx n =
            let go currentParentIx = do
                    minIx <- getIndexOfParentChildrenMinimum array currentParentIx n
                    when (minIx /= currentParentIx) $ do
                        MV.swap array minIx currentParentIx
                        go minIx
            in go parentIx

        createHeap array parentIx
            | parentIx >= 0 = do
                siftDown array parentIx n
                createHeap array (parentIx - 1)
            | otherwise = return ()

        moveElemToItsPlace array k = do
            MV.swap array 0 (k - 1)
            siftDown array 0 (k - 1)

    in runST $ do
        array <- thaw vector
        createHeap array (n `div` 2 - 1)
        mapM_ (moveElemToItsPlace array) [n, (n-1)..(n-k+1)]
        V.reverse <$> freeze (MV.slice (n - k) k array)


main :: IO ()
main = do
    n <- readLn
    vector <- convertToIntVector <$> getLine
    k <- readLn
    let result = partialSort vector n k
    putStrLn $ unwords $ map show $ toList result
