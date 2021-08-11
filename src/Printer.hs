module Printer where

import Control.Monad
import Control.Monad.Fix (mfix)
import Data.Foldable (Foldable (toList), foldlM)
import Data.Functor
import Data.Graph (Graph)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Tree as GV
import GraphViz (GraphViz, GraphVizM)
import qualified GraphViz as GV
import RegExp (RegExp)
import qualified RegExp as RE

joinEdges :: NonEmpty GV.EdgeEndpoint -> RegExp GV.EdgeEndpoint -> GraphVizM (NonEmpty GV.EdgeEndpoint)
joinEdges previously (RE.Exactly a) = forM_ previously (`GV.mkEdge` a) $> return a
joinEdges previously (RE.AnyOf children) = join <$> traverse (joinEdges previously) children
joinEdges previously (RE.SeveralOf children) = foldlM joinEdges previously children
joinEdges previously (RE.ZeroOrOne child) = (previously <>) <$> joinEdges previously child
joinEdges previously (RE.OneOrMore child) = mfix $ (`joinEdges` child) . (previously <>)

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

graphRegExp :: (a -> GraphVizM GV.EdgeEndpoint) -> RegExp a -> GraphVizM (GV.SubgraphRef GV.EdgeEndpoint)
graphRegExp nodeFn regexp = GV.subgraph $ do
  nodes <- traverse nodeFn regexp
  start <- GV.NodeEndpoint <$> startNode
  end <- GV.NodeEndpoint <$> endNode
  lastNodes <- joinEdges (pure start) nodes
  forM_ lastNodes (`GV.mkEdge` end)
  return $ start :| end : toList nodes

graphCharNode :: Char -> GraphVizM GV.EdgeEndpoint
graphCharNode = fmap GV.NodeEndpoint . GV.newNode . T.singleton

graphColourNode :: GV.Colour -> GraphVizM GV.EdgeEndpoint
graphColourNode colour = GV.NodeEndpoint <$> (GV.setNodeFill colour >> GV.newNode "")

graphNestedRegExp :: RegExp (GV.GraphVizM GV.EdgeEndpoint) -> GraphVizM (GV.EdgeEndpoint)
graphNestedRegExp = fmap GV.SubgraphEndpoint . graphRegExp id