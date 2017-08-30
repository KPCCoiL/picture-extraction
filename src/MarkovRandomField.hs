{-# LANGUAGE StrictData #-}
module MarkovRandomField where

import Graph
import Graph.MinCut
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Hashable
import qualified Data.HashSet as S

data UnaryParameter a = UniParam a a
  deriving (Show, Eq, Ord)

data BinaryParameter a = BinParam a a a a
  deriving (Show, Eq, Ord)

data MarkovRandomField a = MarkovRandomField
            { numVertices :: Int
            , unaryParams :: IM.IntMap (UnaryParameter a)
            , binaryParams :: M.Map (Int, Int) (BinaryParameter a)
            , constParam :: a
            } deriving (Show, Eq)

{-
reparameterize :: (Ord a, Num a) => MarkovRandomField a -> MarkovRandomField a
reparameterize mrf@(MarkovRandomField vnum unary binary constant) = mrf
  { unaryParams = newUnis
  , binaryParams = newBins
  , constParam = constant - delta1 - delta2
  }
  where (delta1, newBins) = flip (`M.mapAccum` 0) binary $ \(BinParam zz zo oz oo) acc ->
          let deltaA = minimum [zz, zo, oz, oo]
              [zz', zo', oz', oo'] = map (subtract deltaA) [zz, zo, oz, oo]
              deltaB0 = min zz' oz'
              deltaB1 = min zo' oo'
              [zz'', zo'', oz'', oo''] = [zz' - deltaB0, zo' - deltaB1, oz' - deltaB0, oo' - deltaB1]
              deltaC0 = min zz' zo'
              deltaC1 = min oz' oo'
              [nzz, nzo, noz, noo] = [zz'' - deltaC0, zo'' - deltaC0, oz'' - deltaC1, oo'' - deltaC1]
           in (deltaA + deltaB0 + deltaB1 + deltaC0 + deltaC1 + acc, BinParam nzz nzo noz noo)
        (delta2, newUnis) = flip (`IM.mapAccum` 0) unary $ \(UniParam z o) acc ->
          let delta = min z o in
            (acc + delta, UniParam (z - delta)  (o - delta))
-}
-- FIXME : Nothing but negligence (the code above doesn't typecheck)
reparameterize :: MarkovRandomField a -> MarkovRandomField a
reparameterize = id

minimizeEnergy :: (Ord a, Num a, Hashable a) => MarkovRandomField a -> a -> (a, S.HashSet (Vertex Int))
minimizeEnergy (MarkovRandomField vnum unary binary _) inf = edmondsKarp source sink inf graph
  where source = Vertex $! -2
        sink = Vertex $! -1
        allVertices = [0 .. vnum - 1]
        terminalEdges = do
          i <- allVertices
          let UniParam wSrc wSink = unary IM.! i
          [Edge source wSrc (Vertex i), Edge (Vertex i) wSink sink]
        neighborEdges = do
          i <- allVertices
          j <- allVertices
          case M.lookup (i, j) binary of
            Just (BinParam _ zo _ _) -> return $! Edge (Vertex i) zo (Vertex j)
            Nothing -> []
        allEdges = terminalEdges ++ neighborEdges
        graph = fromEdges allEdges
