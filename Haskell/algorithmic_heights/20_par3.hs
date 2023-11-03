import Control.Monad.ST (runST)
import Data.Vector (Vector, freeze, fromList, thaw, toList)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

convertToIntList :: String -> [Int]
convertToIntList = map read . words


threeWayPartitioning :: Vector Int -> Int -> Int -> Vector Int
threeWayPartitioning vector pivot n =
    let swapElements array =
            let go middleIx currentIx endIx =
                    if currentIx > endIx then return ()
                    else do
                        elem <- MV.read array currentIx
                        if elem < pivot then do
                            MV.swap array currentIx middleIx
                            go (middleIx + 1) (currentIx + 1) endIx
                        else if elem > pivot then do
                            MV.swap array currentIx endIx
                            go middleIx currentIx (endIx - 1)
                        else go middleIx (currentIx + 1) endIx
            in go 0 1 (n - 1)
    in runST $ do
        array <- thaw vector
        swapElements array
        freeze array


main :: IO ()
main = do
    n <- readLn
    xs <- fromList . convertToIntList <$> getLine
    let result = threeWayPartitioning xs (V.head xs) n
    putStrLn $ unwords $ map show $ toList result
