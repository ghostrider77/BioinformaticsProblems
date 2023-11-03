import Control.Monad (replicateM)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import qualified Data.IntMap as M
import qualified Data.IntSet as S

data Edge = Edge Int Int
data Graph = Graph { nrNodes :: Int, adjacencyList :: IntMap IntSet }

convertToIntList :: String -> [Int]
convertToIntList = map read . words


readInputLine :: String -> (Int, Int)
readInputLine line =
    case convertToIntList line of
        [a, b] -> (a, b)
        _ -> error "Malformed input."


createGraph :: Int -> [Edge] -> Graph
createGraph nrNodes edges =
    let addEdge adjacencyList (Edge a b) =
            let update Nothing = Just (S.singleton b)
                update (Just ns) = Just (S.insert b ns)
            in M.alter update a adjacencyList
        addEdgeBothWays adjacencyList edge @ (Edge a b) =
            addEdge (addEdge adjacencyList edge) (Edge b a)
        adjacencyList = foldl addEdgeBothWays M.empty edges
    in Graph { nrNodes = nrNodes, adjacencyList = adjacencyList }


readGraphs :: Int -> IO [Graph]
readGraphs n =
    let readEdges nrEdges = do
            edges <- replicateM nrEdges getLine
            return $ map (\line -> let (a, b) = readInputLine line in Edge a b) edges
        readGraph = do
            _ <- getLine
            (nrNodes, nrEdges) <- readInputLine <$> getLine
            edges <- readEdges nrEdges
            return $ createGraph nrNodes edges
    in mapM (const readGraph) [1..n]


getNeighbors :: Graph -> Int -> IntSet
getNeighbors (Graph _ adjacencyList) node = M.findWithDefault S.empty node adjacencyList


hasCycleOfLengthFour :: Graph -> Bool
hasCycleOfLengthFour graph @ (Graph nrNodes _) =
    let predicate node1 =
            let ns1 = getNeighbors graph node1
                twoCommonNeighbors = any(\node2 ->
                    let ns2 = getNeighbors graph node2
                    in S.size (S.intersection ns2 ns1) >= 2) [(node1+1)..nrNodes]
            in not (S.null ns1) && twoCommonNeighbors
    in any predicate [1..nrNodes]


main :: IO ()
main = do
    n <- readLn
    graphs <- readGraphs n
    let results = map hasCycleOfLengthFour graphs
    putStrLn $ unwords $ map (\res -> if res then "1" else "-1") results
