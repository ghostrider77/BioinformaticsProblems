{-# LANGUAGE TupleSections #-}

import Control.Monad (mapM, replicateM)
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as M
import Data.List (partition)
import Data.Sequence ((><), ViewL ((:< ), EmptyL))
import qualified Data.Sequence as S

type AdjacencyList = IntMap [Int]

data NodeColor = Red | Blue deriving Eq
data Edge = Edge Int Int
data Graph = Graph { nrNodes :: Int, adjacencyList :: AdjacencyList }


convertToIntList :: String -> [Int]
convertToIntList = map read . words


getOppositeColor :: NodeColor -> NodeColor
getOppositeColor Red = Blue
getOppositeColor Blue = Red


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


getNeighbors :: Graph -> Int -> [Int]
getNeighbors (Graph _ adjacencyList) node = M.findWithDefault [] node adjacencyList


findConsistentComponentColoring :: Graph -> Int -> Maybe (IntMap NodeColor)
findConsistentComponentColoring graph startNode =
    let go queue componentColors =
            case S.viewl queue of
                    EmptyL -> Just componentColors
                    (node :< rest) ->
                        let color = componentColors ! node
                            oppositeColor = getOppositeColor color
                            neighbors = getNeighbors graph node
                            (coloredNeighbors, uncoloredNeighbors) = partition (`M.member` componentColors) neighbors
                        in if any (\n -> componentColors ! n == color) coloredNeighbors then Nothing
                            else
                                let newNodes = M.fromList $ map (, oppositeColor) uncoloredNeighbors
                                in go (rest >< S.fromList uncoloredNeighbors) (M.union componentColors newNodes)
    in go (S.singleton startNode) (M.singleton startNode Red)


isBipartite :: Graph -> Bool
isBipartite graph =
    let go nodeColors startNode
            | startNode >= nrNodes graph = True
            | M.member startNode nodeColors = go nodeColors (startNode + 1)
            | otherwise =
                case findConsistentComponentColoring graph startNode of
                    Nothing -> False
                    Just componentColoring -> go (M.union nodeColors componentColoring) (startNode + 1)
    in go M.empty 1


main :: IO ()
main = do
    n <- readLn
    graphs <- readGraphs n
    let results = map isBipartite graphs
    putStrLn $ unwords $ map (\res -> if res then "1" else "-1") results
