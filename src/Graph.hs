module Graph where

import Data.Hashable
import qualified Data.HashMap.Strict as M
import Data.List

newtype Vertex vid = Vertex vid
  deriving (Show, Eq, Ord)

instance Hashable a => Hashable (Vertex a) where
  hashWithSalt s (Vertex x) = hashWithSalt s x

data Edge vid weight = Edge !(Vertex vid) !weight !(Vertex vid)
  deriving (Show, Eq, Ord)

-- newtype Graph vid weight = Graph (M.HashMap vid (S.HashSet (Edge vid weight)))
--   deriving Show

newtype Graph vid weight = Graph (M.HashMap vid (M.HashMap vid weight))
  deriving Show

tupleToEdge :: a -> (a, w) -> Edge a w
tupleToEdge from (to, weight) = Edge (Vertex from) weight (Vertex to)

edgeFrom :: (Eq a, Hashable a) => Graph a w -> Vertex a -> [Edge a w]
edgeFrom (Graph es) (Vertex x) =
  map (tupleToEdge x) $! M.toList $! M.lookupDefault M.empty x es

getWeight :: (Eq a, Hashable a) => Vertex a -> Vertex a -> Graph a w -> Maybe w
getWeight (Vertex from) (Vertex to) (Graph es) = M.lookup from es >>= M.lookup to

toEdges :: (Eq a, Hashable a) => Graph a w -> [Edge a w]
toEdges (Graph es) = concat $! map toEdges' $! M.toList es
  where toEdges' (from, edges) = map (tupleToEdge from) $! M.toList edges

emptyGraph :: Graph a w
emptyGraph = Graph M.empty

fromEdges :: (Eq a, Eq w, Hashable a) => [Edge a w] -> Graph a w
fromEdges = foldl' (flip addEdge) emptyGraph

addEdge :: (Eq a, Eq w, Hashable a) => Edge a w -> Graph a w -> Graph a w
addEdge e@(Edge (Vertex from) weight (Vertex to)) (Graph es) = Graph $! M.alter update from es
  where update Nothing = Just $! M.singleton to weight
        update (Just es) = Just $! M.insert to weight es

delEdge :: (Eq a, Hashable a) => Vertex a -> Vertex a -> Graph a w -> Graph a w
delEdge (Vertex from) (Vertex to) (Graph es) = Graph $! M.adjust (M.delete to) from es

updateEdge :: (Eq a, Hashable a) => Vertex a -> Vertex a -> w -> Graph a w -> Graph a w
updateEdge (Vertex from) (Vertex to) weight (Graph es) = Graph $! M.adjust (M.insert to weight) from es
