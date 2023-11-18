import Control.Monad (mapM_, replicateM)
import Data.IntMap (IntMap, (!))
import Data.List (find)
import qualified Data.IntMap as M

type Component = [Int]

data Edge = Edge Int Int
data Graph = Graph { nrNodes :: Int, adjacencyList :: IntMap [Int] }
data DFSState = DFSState { visitStarted :: IntMap Int
                         , visitEnded :: IntMap Int
                         , previsitId :: Int
                         , postvisitId :: Int
                         , nodeOrdering :: [Int]
                         }
data DFSResult = DFSResult { components :: [Component], topologicalOrdering :: [Int] }

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
            let update Nothing = Just [b]
                update (Just ns) = Just (b : ns)
            in M.alter update a adjacencyList
        adjacencyList = foldl addEdge M.empty edges
    in Graph { nrNodes = nrNodes, adjacencyList = adjacencyList }


readGraphs :: Int -> IO [Graph]
readGraphs n =
    let readEdges nrEdges = do
            edges <- replicateM nrEdges getLine
            return $ map (\line -> let (a, b) = readInputLine line in Edge a b) edges
        readGraph = do
            (nrNodes, nrEdges) <- readInputLine <$> getLine
            edges <- readEdges nrEdges
            return $ createGraph nrNodes edges
    in mapM (const readGraph) [1..n]


pathToString :: Maybe [Int] -> String
pathToString Nothing = "-1"
pathToString (Just result) = "1 " ++ unwords (map show result)


getNeighbors :: Graph -> Int -> [Int]
getNeighbors (Graph _ adjacencyList) node = M.findWithDefault [] node adjacencyList


hasEdge :: Graph -> Int -> Int -> Bool
hasEdge graph node1 node2 =
    node2 `elem` getNeighbors graph node1


depthFirstSearch :: Graph -> DFSResult
depthFirstSearch graph @ (Graph nrNodes adjacencyList) =
    let isNodeVisited state node = M.member node (visitStarted state)
        findUnvisitedNeighbor state node = find (not . isNodeVisited state) $ getNeighbors graph node

        explore state startNode =
            let traverseComponent currentState component [] = (currentState, component)
                traverseComponent currentState component previsitStack @ (node : rest) =
                    case findUnvisitedNeighbor currentState node of
                        Just neighbor ->
                            let visitStarted' = M.insert neighbor (previsitId currentState) (visitStarted currentState)
                                previsitId' = previsitId currentState + 1
                                currentState' = currentState { visitStarted = visitStarted', previsitId = previsitId' }
                            in traverseComponent currentState' (neighbor : component) (neighbor : previsitStack)
                        Nothing ->
                            let visitEnded' = M.insert node (postvisitId currentState) (visitEnded currentState)
                                postvisitId' = postvisitId currentState + 1
                                currentState' = currentState { visitEnded = visitEnded', postvisitId = postvisitId',
                                                               nodeOrdering = node : nodeOrdering currentState }
                            in traverseComponent currentState' component rest
                visitStarted' = M.insert startNode (previsitId state) (visitStarted state)
                previsitId' = previsitId state + 1
            in traverseComponent state {visitStarted = visitStarted', previsitId = previsitId'} [startNode] [startNode]

        findComponents state components [] = (state, components)
        findComponents state components (node : remainingNodes) =
            if isNodeVisited state node then findComponents state components remainingNodes
            else
                let (newState, currentComponent) = explore state node
                in findComponents newState (currentComponent : components) remainingNodes

        initialState = DFSState M.empty M.empty 1 1 []
        (finalState, components) = findComponents initialState [] [1..nrNodes]
    in DFSResult components (nodeOrdering finalState)


findHamiltonianPath :: Graph -> Maybe [Int]
findHamiltonianPath graph =
    let nodes = topologicalOrdering $ depthFirstSearch graph
    in if all (uncurry $ hasEdge graph) $ zip nodes $ tail nodes then Just nodes else Nothing


main :: IO ()
main = do
    n <- readLn
    graphs <- readGraphs n
    let results = map findHamiltonianPath graphs
    mapM_ (putStrLn . pathToString) results
