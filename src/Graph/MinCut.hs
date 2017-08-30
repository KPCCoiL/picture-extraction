{-# LANGUAGE BangPatterns #-}
module Graph.MinCut where

import Graph
import Data.Hashable
import qualified Data.HashSet as S
import Data.List

data Flow a = Flow !a !a
  deriving (Show, Eq, Ord)

type FlowNetwork a f = Graph a (Flow f)

residualFlow :: Num a => Flow a -> Flow a
residualFlow (Flow cap cur) = Flow cap (cap - cur)

addFlow :: Num a => a -> Flow a -> Flow a
addFlow inc (Flow cap cur) = Flow cap (cur + inc)

residualNetwork :: (Eq a, Hashable a, Eq f, Num f) => FlowNetwork a f -> FlowNetwork a f
residualNetwork = fromEdges . concat . map residual . toEdges
  where residual e@(Edge from flow to) = [e, Edge to (residualFlow flow) from]

-- Edmonds Karp Algorithm

breadthFirstSearch :: (Eq a, Hashable a) => ((Vertex a, b) -> [(Vertex a, b)]) -> Vertex a -> b -> [(Vertex a, b)]
breadthFirstSearch getNexts start dat = concat $!
  takeWhile (not . null) $!
  iterate (concatMap getNexts) [(start, dat)]

searchIncPath :: (Eq a, Hashable a, Ord f, Num f) => Vertex a -> Vertex a -> f -> FlowNetwork a f -> Maybe ([Vertex a], f)
searchIncPath src sink inf net = extract <$> find isPath queue
  where isPath (v, _) = v == sink
        extract (_, (path, _, cap)) = (reverse path, cap)
        queue = breadthFirstSearch getNexts src ([src], S.singleton src, inf)
        getNexts (!pos, (!path, !past, !flow)) = [ (next, (next : path, S.insert next past, min (cap - lim) flow))
                                                 | (Edge _ (Flow cap lim) next) <- net `edgeFrom` pos
                                                 , cap > lim
                                                 , not $! next `S.member` past ]

increaseFlow :: (Eq a, Hashable a, Ord f, Num f) => [Vertex a] -> f -> FlowNetwork a f -> FlowNetwork a f
increaseFlow path inc net = foldl' increase net $! zip path $! tail path
  where increase graph (from, to) =
          let Just (Flow cap cur) = getWeight from to graph
              newFlow = Flow cap (cur + inc) in
            updateEdge from to newFlow $!
            updateEdge to from (residualFlow newFlow) graph

-- minimumCut :: source -> sink -> graph -> (maximum flow, vertexes)
minimumCut :: (Eq a, Hashable a, Ord f, Num f) => Vertex a -> Vertex a -> f -> FlowNetwork a f -> (f, S.HashSet (Vertex a))
minimumCut src sink inf net = (maxFlow, snd $! last queue)
  where go !graph !flow = case searchIncPath src sink inf graph of
          Nothing -> (graph, flow)
          Just (!path, !inc) -> go (increaseFlow path inc graph) (flow + inc)
        (finalNet, maxFlow) = go net 0
        queue = breadthFirstSearch getNexts src $! S.singleton src
        getNexts (!pos, !past) = [ (next, S.insert next past)
                                 | (Edge _ (Flow cap lim) next) <- finalNet `edgeFrom` pos
                                 , cap > lim
                                 , not $! next `S.member` past ]


edmondsKarp :: (Eq a, Hashable a, Ord f, Num f) => Vertex a -> Vertex a -> f -> Graph a f -> (f, S.HashSet (Vertex a))
edmondsKarp source sink inf = minimumCut source sink inf . residualNetwork . graphToFlowNet
  where graphToFlowNet = fromEdges . map edgeToTube . toEdges
        edgeToTube (Edge v w u) = Edge v (Flow w 0) u
