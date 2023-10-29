import Control.Monad (replicateM)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as H
import Data.IntSet (IntSet)
import qualified Data.IntSet as S

data Edge = Edge { nodeFrom :: Int, nodeTo :: Int, weight :: Int }
type AdjacencyList = IntMap [Edge]
data Graph = Graph { nrNodes :: Int, adjacencyList :: AdjacencyList }

data Distance = Dist Int | Infinity deriving Eq
data State = State { queue :: MinPrioHeap Distance Int, distances :: IntMap Distance, finalizedNodes :: IntSet }


instance Ord Distance where
    Infinity <= Infinity = True
    Dist d1 <= Dist d2 = d1 <= d2
    Infinity <= Dist _ = False
    Dist _ <= Infinity = True


instance Show Distance where
    show (Dist d) = show d
    show Infinity = "-1"


addDist :: Distance -> Distance -> Distance
addDist (Dist d1) (Dist d2) = Dist (d1 + d2)
addDist _ _ = Infinity


convertToIntList :: String -> [Int]
convertToIntList = map read . words


readInputLine :: String -> (Int, Int)
readInputLine line =
    case convertToIntList line of
        [a, b] -> (a, b)
        _ -> error "Malformed input."


readEdges :: Int -> IO [Edge]
readEdges nrEdges = do
    let parse line =
            case convertToIntList line of
                [a, b, w] -> Edge a b w
                _ -> error "Malformed edge input."
    edges <- replicateM nrEdges getLine
    return $ map parse edges


createGraph :: Int -> [Edge] -> Graph
createGraph nrNodes edges =
    let addEdge adjacencyList edge =
            let update Nothing = Just [edge]
                update (Just ns) = Just (edge : ns)
            in M.alter update (nodeFrom edge) adjacencyList
        adjacencyList = foldl addEdge M.empty edges
    in Graph { nrNodes = nrNodes, adjacencyList = adjacencyList }


getNeighbors :: Graph -> Int -> [Edge]
getNeighbors (Graph _ adjacencyList) node = M.findWithDefault [] node adjacencyList


updateDistances :: State -> [Edge] -> Distance -> State
updateDistances state neighbors dist =
    let go currentState [] = currentState
        go currentState @ (State queue distances _) ((Edge _ v weight) : ns) =
            let distanceThroughNode = addDist dist (Dist weight)
                currentDistance = M.findWithDefault Infinity v distances
            in if currentDistance <= distanceThroughNode then go currentState ns
            else
                let queue' = H.insert (distanceThroughNode, v) queue
                    distances' =  M.insert v distanceThroughNode distances
                in go (currentState { queue = queue', distances = distances' }) ns
    in go state neighbors


calcShortestDistances :: Graph -> Int -> [Distance]
calcShortestDistances graph @ (Graph nrNodes adjacencyList) sourceNode =
    let go state @ (State queue distances finalizedNodes) =
            case H.view queue of
                Nothing -> map (\node -> M.findWithDefault Infinity node distances) [1..nrNodes]
                Just ((dist, node), rest) ->
                    if S.member node finalizedNodes then go (State rest distances finalizedNodes)
                    else
                        let neighbors = getNeighbors graph node
                            state' = updateDistances state neighbors dist
                        in go state' { finalizedNodes = S.insert node finalizedNodes }
    in go $ State (H.fromList [(Dist 0, sourceNode)]) ( M.singleton sourceNode (Dist 0)) S.empty


main :: IO ()
main = do
    (nrNodes, nrEdges) <- readInputLine <$> getLine
    edges <- readEdges nrEdges
    let graph = createGraph nrNodes edges
    let result = calcShortestDistances graph 1
    putStrLn $ unwords $ map show result
