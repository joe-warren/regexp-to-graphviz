module Printer where

import Control.Monad
import Control.Monad.Fix (mfix)
import Data.Foldable (Foldable (toList), foldlM)
import Data.Functor
import Data.Graph (Graph)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import GraphViz (GraphViz, GraphVizM)
import qualified GraphViz as GV
import RegExp (RegExp)
import qualified RegExp as RE

joinEdges :: (e -> e -> GraphVizM ()) -> NonEmpty e -> RegExp e -> GraphVizM (NonEmpty e)
joinEdges mkEdge previously (RE.Exactly a) = forM_ previously (`mkEdge` a) $> return a
joinEdges mkEdge previously (RE.AnyOf children) = join <$> traverse (joinEdges mkEdge previously) children
joinEdges mkEdge previously (RE.SeveralOf children) = foldlM (joinEdges mkEdge) previously children
joinEdges mkEdge previously (RE.ZeroOrOne child) = (previously <>) <$> joinEdges mkEdge previously child
joinEdges mkEdge previously (RE.OneOrMore child) = mfix ((\x -> joinEdges mkEdge x child) . (previously <>))

startNode :: GraphVizM GV.NodeRef
startNode = do
  GV.setNodeFill $ GV.Colour "white"
  GV.setNodeShape GV.DoubleCircle
  GV.newNode ""

endNode :: GraphVizM GV.NodeRef
endNode = do
  GV.setNodeFill $ GV.Colour "black"
  GV.setNodeShape GV.Circle
  GV.newNode ""

graphRegExp :: GV.EdgeConstructors e -> (a -> GraphVizM e) -> RegExp a -> GraphVizM (GV.SubgraphRef e)
graphRegExp cons nodeFn regexp = GV.subgraph $ do
  nodes <- traverse nodeFn regexp
  start <- GV.liftNodeRef cons startNode
  end <- GV.liftNodeRef cons endNode
  let mkEdge = GV.mkEdge cons
  lastNodes <- joinEdges mkEdge (pure start) nodes
  forM_ lastNodes (`mkEdge` end)
  return $ start :| end : toList nodes

graphCharRegExp :: RegExp Char -> GraphVizM (GV.SubgraphRef GV.NodeRef)
graphCharRegExp = graphRegExp GV.nodeRefEdgeConstructors (GV.newNode . T.singleton)

graphColourRegExp :: RegExp GV.Colour -> GraphVizM (GV.SubgraphRef GV.NodeRef)
graphColourRegExp = graphRegExp GV.nodeRefEdgeConstructors $ \colour -> GV.setNodeFill colour >> GV.newNode ""

graphNestedRegExp :: GV.EdgeConstructors (GV.SubgraphRef a) -> RegExp (GV.GraphVizM (GV.SubgraphRef a)) -> GraphVizM (GV.SubgraphRef (GV.SubgraphRef a))
graphNestedRegExp cons = graphRegExp cons id