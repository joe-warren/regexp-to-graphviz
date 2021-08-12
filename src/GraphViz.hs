module GraphViz where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Writer.Strict (MonadWriter)
import Data.Bool (bool)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Base (NonEmpty)

newtype GraphViz = GraphViz {getGraphVizSource :: Text} deriving (Semigroup, Monoid) via Text

newtype Counter = Counter {unCounter :: Integer} deriving (Show) via Integer

incrementCounter :: Counter -> Counter
incrementCounter = Counter . (+ 1) . unCounter

getUID :: GraphVizM Counter
getUID = do
  uid <- get
  modify incrementCounter
  return uid

newtype GraphVizM a = GraphVisM {unGraphVizM :: StateT Counter (Writer GraphViz) a}
  deriving (Functor, Applicative, Monad, MonadState Counter, MonadWriter GraphViz, MonadFix) via (StateT Counter (Writer GraphViz))

runGraphVizM :: GraphVizM () -> GraphViz
runGraphVizM = execWriter . (`evalStateT` Counter 0) . unGraphVizM . digraph

newtype NodeRef = NodeRef {getRawReference :: Text}

data SubgraphRef nodes = SubgraphRef
  { getRawSubgraphReference :: Text,
    subgraphNodes :: NonEmpty nodes
  }

data EdgeEndpoint = NodeEndpoint NodeRef | SubgraphEndpoint (SubgraphRef EdgeEndpoint)

digraph :: GraphVizM () -> GraphVizM NodeRef
digraph g = do
  counter <- getUID
  let rawId = "fsm_" <> (T.pack . show $ counter)
  tell . GraphViz . T.unlines $
    [ T.unwords ["digraph", rawId, "{"],
      "compound=true;",
      "rankdir=LR;",
      "size=\"8,5\""
    ]
  g
  tell $ GraphViz "}\n"
  pure . NodeRef $ rawId

subgraph' :: Bool -> GraphVizM (NonEmpty a) -> GraphVizM (SubgraphRef a)
subgraph' isCluster g = do
  counter <- getUID
  let prefix = bool "_" "cluster_" isCluster
  let rawId = prefix <> (T.pack . show $ counter)
  tell . GraphViz . T.unlines $
    [ T.unwords ["subgraph", rawId, "{"],
      "rankdir=LR;",
      "size=\"8,5\""
    ]
  setNodeShape Circle
  childNodes <- g
  tell $ GraphViz "\n}\n"
  pure $ SubgraphRef rawId childNodes

subgraph :: GraphVizM (NonEmpty a) -> GraphVizM (SubgraphRef a)
subgraph = subgraph' False

cluster = subgraph' True

newNode :: Text -> GraphVizM NodeRef
newNode name = do
  counter <- getUID
  let rawId = T.concat [name, "_", T.pack . show $ counter]
  tell . GraphViz $ T.concat [rawId, "[label = \"", name, "\"];\n"]
  pure . NodeRef $ rawId

getSomeNodeRef :: EdgeEndpoint -> NodeRef
getSomeNodeRef (NodeEndpoint n) = n
getSomeNodeRef (SubgraphEndpoint s) = getSomeNodeRef . NE.head . subgraphNodes $ s

mkEdge :: EdgeEndpoint -> EdgeEndpoint -> GraphVizM ()
mkEdge (NodeEndpoint a) (NodeEndpoint b) = tell . GraphViz . T.concat $ [getRawReference a, " -> ", getRawReference b, ";\n"]
mkEdge a b = do
  pointId <- ("_" <>) . T.pack . show <$> getUID
  tell . GraphViz . T.concat $
    [ pointId,
      "[shape = point];\n"
    ]
  tell . GraphViz . T.concat $
    [ getRawReference . getSomeNodeRef $ a,
      " -> ",
      pointId,
      "[dir = none, ltail = ",
      subgraphReference a,
      "];\n",
      pointId,
      " -> ",
      getRawReference . getSomeNodeRef $ b,
      "[ lhead = ",
      subgraphReference b,
      "];\n"
    ]
  where
    subgraphReference (NodeEndpoint _) = "\"\""
    subgraphReference (SubgraphEndpoint (SubgraphRef ref _)) = ref

newtype Colour = Colour {rawColour :: Text}

data Shape = Circle | DoubleCircle

setNodeShape :: Shape -> GraphVizM ()
setNodeShape s =
  tell . GraphViz . T.concat $
    [ "node[ shape = ",
      name s,
      "];\n"
    ]
  where
    name Circle = "circle"
    name DoubleCircle = "doublecircle"

setNodeFill :: Colour -> GraphVizM ()
setNodeFill c =
  tell . GraphViz . T.concat $
    [ "node [style=filled, fillcolor = \"",
      rawColour c,
      "\"];\n"
    ]
