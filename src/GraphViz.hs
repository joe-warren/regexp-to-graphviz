module GraphViz where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Writer.Strict (MonadWriter)
import Data.Text (Text)
import qualified Data.Text as T

newtype GraphViz = GraphViz {getGraphvizSource :: Text} deriving (Semigroup, Monoid) via Text

newtype Counter = Counter {unCounter :: Integer} deriving (Show) via Integer

incrementCounter :: Counter -> Counter
incrementCounter = Counter . (+ 1) . unCounter

newtype GraphVizM a = GraphVisM {unGraphVizM :: StateT Counter (Writer GraphViz) a}
  deriving (Functor, Applicative, Monad, MonadState Counter, MonadWriter GraphViz, MonadFix) via (StateT Counter (Writer GraphViz))

runGraphVizM :: GraphVizM () -> GraphViz
runGraphVizM = wrap . execWriter . (`evalStateT` (Counter 0)) . unGraphVizM . (setDefaults >>)
  where
    setDefaults = setNodeShape Circle >> setNodeFill (Colour "white")
    wrap contents =
      GraphViz . T.unlines $
        [ "digraph finite_state_machine {",
          "rankdir=LR;",
          "size=\"8,5\"",
          getGraphvizSource contents,
          "}"
        ]

newtype NodeRef = NodeRef {getRawReference :: Text}

newNode :: Text -> GraphVizM NodeRef
newNode name = do
  counter <- get
  modify incrementCounter
  let rawId = T.concat [name, "_", T.pack . show $ counter]
  tell . GraphViz $ T.concat [rawId, "[label = \"", name, "\"];\n"]
  pure . NodeRef $ rawId

mkEdge :: NodeRef -> NodeRef -> GraphVizM ()
mkEdge a b = tell . GraphViz . T.concat $ [getRawReference a, " -> ", getRawReference b, ";\n"]

newtype Colour = Colour {rawColour :: Text}

data Shape = Circle | DoubleCircle

setNodeShape :: Shape -> GraphVizM ()
setNodeShape s =
  tell . GraphViz . T.concat $
    [ "node[ shape = ",
      name s,
      "];"
    ]
  where
    name Circle = "circle"
    name DoubleCircle = "doublecircle"

setNodeFill :: Colour -> GraphVizM ()
setNodeFill c =
  tell . GraphViz . T.concat $
    [ "node [style=filled, fillcolor = ",
      rawColour c,
      "];\n"
    ]
