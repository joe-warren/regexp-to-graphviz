module Lib
  ( someFunc,
  )
where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable (forM)
import qualified GraphViz as GV
import qualified Printer as P
import qualified RegExp as RE
import System.Environment (getArgs)
import System.IO (stderr)

someFunc :: IO ()
someFunc = do
  args <- fmap T.pack <$> getArgs
  forM_ args $ \a -> case RE.parseRegExp a of
    Left err -> T.hPutStrLn stderr err
    Right regExp -> T.putStrLn . GV.getGraphvizSource . P.graphRegExp $ regExp
