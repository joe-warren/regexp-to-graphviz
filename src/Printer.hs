module Printer where

import Control.Monad
import Data.Foldable (foldlM)
import Data.Functor
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import GraphViz (GraphViz, GraphVizM)
import qualified GraphViz as GV
import RegExp (RegExp)
import qualified RegExp as RE

headNodes :: RegExp a -> NonEmpty a
headNodes (RE.Exactly a) = return a
headNodes (RE.AnyOf children) = children >>= headNodes
headNodes (RE.SeveralOf children) = headNodes $ NE.head children
headNodes (RE.ZeroOrOne child) = headNodes child
headNodes (RE.OneOrMore child) = headNodes child

joinEdges :: NonEmpty GV.NodeRef -> RegExp GV.NodeRef -> GraphVizM (NonEmpty GV.NodeRef)
joinEdges previously (RE.Exactly a) = forM_ previously (`GV.mkEdge` a) $> return a
joinEdges previously (RE.AnyOf children) = join <$> traverse (joinEdges previously) children
joinEdges previously (RE.SeveralOf children) = foldlM joinEdges previously children
joinEdges previously (RE.ZeroOrOne child) = (previously <>) <$> joinEdges previously child
joinEdges previously (RE.OneOrMore child) = do
  lastNodes <- joinEdges previously child
  forM_ lastNodes $ \s -> forM_ (headNodes child) (GV.mkEdge s)
  return lastNodes

graphRegExp :: [RegExp Char] -> GraphViz
graphRegExp regexp = GV.runGraphVizM $ do
  GV.setNodeFill $ GV.Colour "white"
  nodes <- traverse (traverse (GV.newNode . T.singleton)) regexp
  GV.setNodeShape GV.DoubleCircle
  start <- GV.newNode ""
  GV.setNodeFill $ GV.Colour "black"
  GV.setNodeShape GV.Circle
  end <- GV.newNode ""
  lastNodes <- foldlM joinEdges (pure start) nodes
  forM_ lastNodes (`GV.mkEdge` end)
  return ()