import Control.Monad (replicateM)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

data Edge = Edge Int Int
data Graph = Graph { nrNodes :: Int, adjacencyList :: IntMap [Int] }

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


getNodeDegree :: Graph -> Int -> Int
getNodeDegree (Graph _ adjacencyList) node =
    let neighbors = M.findWithDefault [] node adjacencyList
    in length neighbors


calcNodeDegrees :: Int -> [Edge] -> [Int]
calcNodeDegrees nrNodes edges =
    let graph = createGraph nrNodes edges
    in map (getNodeDegree graph) [1..nrNodes]


main :: IO ()
main = do
    (nrNodes, nrEdges) <- readInputLine <$> getLine
    edges <- readEdges nrEdges
    let result = calcNodeDegrees nrNodes edges
    putStrLn $ unwords $ map show result
