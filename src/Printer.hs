module Printer where

import Control.Monad
import Control.Monad.Fix (mfix)
import Data.Foldable (foldlM)
import Data.Functor
import Data.Graph (Graph)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import GraphViz (GraphViz, GraphVizM)
import qualified GraphViz as GV
import RegExp (RegExp)
import qualified RegExp as RE

joinEdges :: NonEmpty GV.NodeRef -> RegExp GV.NodeRef -> GraphVizM (NonEmpty GV.NodeRef)
joinEdges previously (RE.Exactly a) = forM_ previously (`GV.mkEdge` a) $> return a
joinEdges previously (RE.AnyOf children) = join <$> traverse (joinEdges previously) children
joinEdges previously (RE.SeveralOf children) = foldlM joinEdges previously children
joinEdges previously (RE.ZeroOrOne child) = (previously <>) <$> joinEdges previously child
joinEdges previously (RE.OneOrMore child) = mfix ((`joinEdges` child) . (previously <>))

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

graphRegExp :: (a -> GraphVizM GV.NodeRef) -> RegExp a -> GraphViz
graphRegExp nodeFn regexp = GV.runGraphVizM $ do
  nodes <- traverse nodeFn regexp
  start <- startNode
  end <- endNode
  lastNodes <- joinEdges (pure start) nodes
  forM_ lastNodes (`GV.mkEdge` end)

graphCharRegExp :: RegExp Char -> GraphViz
graphCharRegExp = graphRegExp (GV.newNode . T.singleton)

graphColourRegExp :: RegExp GV.Colour -> GraphViz
graphColourRegExp = graphRegExp $ \colour -> GV.setNodeFill colour >> GV.newNode ""