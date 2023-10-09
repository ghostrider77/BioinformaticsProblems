import Control.Monad (replicateM)
import Data.IntMap (IntMap, (!))
import Data.Maybe (fromMaybe)
import Data.Sequence ((|>), ViewL ((:< ), EmptyL))
import qualified Data.IntMap as Map
import qualified Data.Sequence as S

type AdjacencyList = IntMap [Int]
data Edge = Edge Int Int
data DirectedGraph = DirectedGraph { nrNodes :: Int, adjacencyList :: AdjacencyList }

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


createGraph :: Int -> [Edge] -> DirectedGraph
createGraph nrNodes edges =
    let addEdge adjacencyList (Edge a b) =
            let update Nothing = Just [b]
                update (Just ns) = Just (b : ns)
            in Map.alter update a adjacencyList
        adjacencyList = foldl addEdge Map.empty edges
    in DirectedGraph { nrNodes = nrNodes, adjacencyList = adjacencyList }


getNeighbors :: DirectedGraph -> Int -> [Int]
getNeighbors (DirectedGraph _ adjacencyList) node =
    fromMaybe [] $ Map.lookup node adjacencyList


breadthFirstSearch :: DirectedGraph -> Int -> [Int]
breadthFirstSearch graph @ (DirectedGraph nrNodes adjacencyList) startNode =
    let go queue distances =
            case S.viewl queue of
                EmptyL -> map (\node -> Map.findWithDefault (-1) node distances) [1..nrNodes]
                (node :< rest) ->
                    let nodeDistance = distances ! node
                        update [] q dists = (q, dists)
                        update (neighbor : ns) q dists =
                            if Map.member neighbor dists then update ns q dists
                            else update ns (q |> neighbor) (Map.insert neighbor (nodeDistance + 1) dists)
                        neighbors = getNeighbors graph node
                        (queue', distances') = update neighbors rest distances
                    in go queue' distances'
    in go (S.singleton startNode) (Map.singleton startNode 0)


main :: IO ()
main = do
    (nrNodes, nrEdges) <- readInputLine <$> getLine
    edges <- readEdges nrEdges
    let graph = createGraph nrNodes edges
    let result = breadthFirstSearch graph 1
    putStrLn $ unwords $ map show result
