module Lib
  ( someFunc,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Data
import Data.Foldable
import Data.Functor.Compose
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable (forM)
import qualified GraphViz as GV
import qualified Options.Applicative as OA
import Options.Applicative.Types (Parser)
import qualified Printer as P
import qualified RegExp as RE
import System.Environment (getArgs)
import System.IO (stderr)

data GraphType = Character | Colour | Nested GraphType deriving (Show)

data Options = Options
  { graphType :: GraphType,
    outputAction :: T.Text -> IO (),
    input :: [T.Text]
  }

typeParser :: OA.Parser GraphType
typeParser = iter Nested <$> nestDepthParser <*> unnestedParser
  where
    iter f 0 v = v
    iter f n v = iter f (n - 1) (f v)
    nestDepthParser = length <$> (many $ OA.flag' () (OA.long "nested"))
    unnestedParser =
      OA.flag' Colour (OA.long "colour")
        <|> OA.flag' Character (OA.long "character")
        <|> pure Character

outputActionParser :: OA.Parser (Text -> IO ())
outputActionParser =
  T.writeFile <$> OA.strOption (OA.long "output" <> OA.short 'o')
    <|> pure T.putStr

optionsParser :: OA.ParserInfo Options
optionsParser =
  OA.info
    (parser <**> OA.helper)
    (OA.fullDesc <> OA.progDesc "generate a dot file from a regexp")
  where
    parser =
      Options
        <$> typeParser
        <*> outputActionParser
        <*> some (OA.strArgument (OA.help "regexp"))

handleError :: (a -> IO ()) -> Either Text a -> IO ()
handleError =
  either
    (T.hPutStrLn stderr)

doParseAndPrint :: RE.Parser (GV.GraphVizM GV.EdgeEndpoint) -> T.Text -> Either T.Text (GV.GraphVizM ())
doParseAndPrint parser = fmap (void . P.graphRegExp id) . RE.parseRegExp parser

parserFor :: GraphType -> RE.Parser (GV.GraphVizM GV.EdgeEndpoint)
parserFor Character = P.graphCharNode <$> RE.charRegExpParser
parserFor Colour = P.graphColourNode . GV.Colour <$> RE.braceRegExParser
parserFor (Nested subtype) = P.graphNestedRegExp <$> RE.nestedParser (parserFor subtype)

someFunc :: IO ()
someFunc = do
  (Options t output args) <- OA.execParser optionsParser
  let process = doParseAndPrint . parserFor $ t
  handleError output $
    fmap (GV.getGraphVizSource . GV.runGraphVizM . void) $
      getCompose $
        traverse_
          (Compose . process)
          args
