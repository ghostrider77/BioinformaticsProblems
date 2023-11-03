import Control.Monad.ST (runST)
import Data.Vector (Vector, freeze, fromList, thaw, toList)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

convertToIntList :: String -> [Int]
convertToIntList = map read . words


twoWayPartitioning :: Vector Int -> Int -> Int -> Vector Int
twoWayPartitioning vector pivot n =
    let swapElements array =
            let go startIx endIx =
                    if startIx > endIx then return ()
                    else do
                        elem <- MV.read array startIx
                        if elem < pivot then do
                            MV.swap array startIx (startIx - 1)
                            go (startIx + 1) endIx
                        else do
                            MV.swap array startIx endIx
                            go startIx (endIx - 1)
            in go 1 (n - 1)
    in runST $ do
        array <- thaw vector
        swapElements array
        freeze array


main :: IO ()
main = do
    n <- readLn
    xs <- fromList . convertToIntList <$> getLine
    let result = twoWayPartitioning xs (V.head xs) n
    putStrLn $ unwords $ map show $ toList result
