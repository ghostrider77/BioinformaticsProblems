import Control.Monad (replicateM)
import Data.IntMap (IntMap)
import Data.Maybe (fromMaybe)
import qualified Data.IntMap as M

type AdjacencyList = IntMap [Int]
data Edge = Edge Int Int
data Graph = Graph { nrNodes :: Int, adjacencyList :: AdjacencyList }

convertToIntList :: String -> [Int]
convertToIntList = map read . words


readInputLine :: String -> (Int, Int)
readInputLine line =
    case convertToIntList line of
        [a, b] -> (a, b)
        _ -> error "Malformed input."


readEdges :: Int -> IO [Edge]
readEdges nrEdges = do
    edges <- replicateM nrEdges getLine
    return $ map (\line -> let (a, b) = readInputLine line in Edge a b) edges


createGraph :: Int -> [Edge] -> Graph
createGraph nrNodes edges =
    let addEdge adjacencyList (Edge a b) =
            let update Nothing = Just [b]
                update (Just ns) = Just (b : ns)
            in M.alter update a adjacencyList
        addEdgeBothWays adjacencyList edge @ (Edge a b) =
            addEdge (addEdge adjacencyList edge) (Edge b a)
        adjacencyList = foldl addEdgeBothWays M.empty edges
    in Graph { nrNodes = nrNodes, adjacencyList = adjacencyList }


getNeighbors :: Graph -> Int -> [Int]
getNeighbors (Graph _ adjacencyList) node = M.findWithDefault [] node adjacencyList


calcDoubleDegreeArray :: Int -> [Edge] -> [Int]
calcDoubleDegreeArray nrNodes edges =
    let graph = createGraph nrNodes edges
        degrees = M.map length (adjacencyList graph)
        getDegree node = fromMaybe 0 (M.lookup node degrees)
    in map (foldl (\acc n -> acc + getDegree n) 0 . getNeighbors graph) [1..nrNodes]


main :: IO ()
main = do
    (nrNodes, nrEdges) <- readInputLine <$> getLine
    edges <- readEdges nrEdges
    let result = calcDoubleDegreeArray nrNodes edges
    putStrLn $ unwords $ map show result
