import Control.Monad (replicateM)
import Data.IntMap (IntMap, (!))
import Data.List (find)
import qualified Data.IntMap as M

type Component = [Int]

data Edge = Edge Int Int
data Graph = Graph { nrNodes :: Int, adjacencyList :: IntMap [Int] }
data DFSState = DFSState { visitStarted :: IntMap Int, visitEnded :: IntMap Int, previsitId :: Int, postvisitId :: Int }
data DFSResult = DFSResult { components :: [Component], previsitIds :: IntMap Int, postvisitIds :: IntMap Int }

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
            _ <- getLine
            (nrNodes, nrEdges) <- readInputLine <$> getLine
            edges <- readEdges nrEdges
            return $ createGraph nrNodes edges
    in mapM (const readGraph) [1..n]


getNeighbors :: Graph -> Int -> [Int]
getNeighbors (Graph _ adjacencyList) node = M.findWithDefault [] node adjacencyList


getConnectedComponents :: Graph -> DFSResult
getConnectedComponents graph @ (Graph nrNodes adjacencyList) =
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
                                currentState' = currentState { visitEnded = visitEnded', postvisitId = postvisitId' }
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

        initialState = DFSState M.empty M.empty 1 1
        (finalState, components) = findComponents initialState [] [1..nrNodes]
    in DFSResult components (visitStarted finalState) (visitEnded finalState)


isAcyclic :: Graph -> Bool
isAcyclic graph @ (Graph _ adjacencyList) =
    let DFSResult { postvisitIds = postvisitIds } = getConnectedComponents graph
        checkProperty node neighbors =
            let nodeNumber = postvisitIds ! node
            in all (\neighbor -> (postvisitIds ! neighbor) < nodeNumber) neighbors
    in all (uncurry checkProperty) $ M.toList adjacencyList


main :: IO ()
main = do
    n <- readLn
    graphs <- readGraphs n
    let results = map isAcyclic graphs
    putStrLn $ unwords $ map (\res -> if res then "1" else "-1") results
