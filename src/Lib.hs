module Lib
  ( someFunc,
  )
where

import Control.Applicative
import Control.Monad
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable (forM)
import qualified GraphViz as GV
import qualified Options.Applicative as OA
import qualified Printer as P
import qualified RegExp as RE
import System.Environment (getArgs)
import System.IO (stderr)

data GraphType = Character | Colour

data Options = Options
  { graphType :: GraphType,
    outputAction :: T.Text -> IO (),
    input :: [T.Text]
  }

typeParser :: OA.Parser GraphType
typeParser =
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
    parser
    (OA.fullDesc <> OA.progDesc "generate a dot file from a regexp")
  where
    parser =
      Options
        <$> typeParser
        <*> outputActionParser
        <*> some (OA.strArgument (OA.help "regexp"))

someFunc :: IO ()
someFunc = do
  (Options t output args) <- OA.execParser optionsParser
  let process = case t of
        Character -> fmap P.graphCharRegExp . RE.parseCharRegExp
        Colour -> fmap (P.graphColourRegExp . fmap (fmap GV.Colour)) . RE.parseBraceRegExp
  forM_ args $ \a -> case process a of
    Left err -> T.hPutStrLn stderr err
    Right gv -> output . GV.getGraphvizSource $ gv
