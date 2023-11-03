import Control.Monad (replicateM)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import qualified Data.IntMap as M
import qualified Data.IntSet as S

data Edge = Edge { nodeFrom :: Int, nodeTo :: Int, weight :: Int }
data Graph = Graph { nrNodes :: Int, adjacencyList :: IntMap [Edge] }

data Distance = Dist Int | Infinity deriving Eq

instance Ord Distance where
    Infinity <= Infinity = True
    Dist d1 <= Dist d2 = d1 <= d2
    Infinity <= Dist _ = False
    Dist _ <= Infinity = True


instance Show Distance where
    show (Dist d) = show d
    show Infinity = "x"


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


updateDistances :: Graph -> IntMap Distance -> IntMap Distance
updateDistances graph distances =
    let updateSingleDistance dist currentDistances (Edge _ neighbor weight) =
            let distThroughNode = addDist dist (Dist weight)
                neighborDist = M.findWithDefault Infinity neighbor currentDistances
            in if neighborDist > distThroughNode then M.insert neighbor distThroughNode currentDistances
            else currentDistances
        update node neighbors currentDistances =
            case M.lookup node currentDistances of
                Nothing -> currentDistances
                Just distNode -> foldl (updateSingleDistance distNode) currentDistances neighbors
    in M.foldrWithKey update distances (adjacencyList graph)


bellmanFord :: Graph -> Int -> [Distance]
bellmanFord graph @ (Graph nrNodes adjacencyList) sourceNode =
    let distances = foldl (\acc _ -> updateDistances graph acc) (M.singleton sourceNode (Dist 0)) [1..nrNodes]
    in map (\node -> M.findWithDefault Infinity node distances) [1..nrNodes]


main :: IO ()
main = do
    (nrNodes, nrEdges) <- readInputLine <$> getLine
    edges <- readEdges nrEdges
    let graph = createGraph nrNodes edges
    let result = bellmanFord graph 1
    putStrLn $ unwords $ map show result
