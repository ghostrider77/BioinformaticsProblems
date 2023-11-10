import Control.Monad.ST (runST)
import Data.Vector (Vector, fromList, thaw)
import System.Random (randomR, mkStdGen)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

convertToIntList :: String -> [Int]
convertToIntList = map read . words


findKthSmallestElement :: Vector Int -> Int -> Int -> Int
findKthSmallestElement vector n k =
    let threeWayPartitioning array pivot startIx endIx =
            let swapElems currentIx startIx endIx =
                    if currentIx > endIx then return (startIx, endIx + 1)
                    else do
                        elem <- MV.read array currentIx
                        if elem < pivot then do
                            MV.swap array currentIx startIx
                            swapElems (currentIx + 1) (startIx + 1) endIx
                        else if elem > pivot then do
                            MV.swap array currentIx endIx
                            swapElems currentIx startIx (endIx - 1)
                        else swapElems (currentIx + 1) startIx endIx
            in swapElems startIx startIx endIx

        go array startIx endIx gen = do
            let (randomIx, gen') = randomR (startIx, endIx) gen
            pivot <- MV.read array randomIx
            (middleStart, middleEnd) <- threeWayPartitioning array pivot startIx endIx
            if k <= middleStart then go array startIx middleStart gen'
            else if k <= middleEnd then MV.read array middleStart
            else go array middleEnd endIx gen'

    in runST $ do
        array <- thaw vector
        let gen = mkStdGen 2112
        go array 0 (n - 1) gen


main :: IO ()
main = do
    n <- readLn
    xs <- fromList . convertToIntList <$> getLine
    k <- readLn
    print $ findKthSmallestElement xs n k
