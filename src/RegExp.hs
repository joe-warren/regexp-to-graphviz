module RegExp where

import Control.Applicative
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (Traversable)
import Data.Void (Void)
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P

data RegExp a
  = SeveralOf (NonEmpty (RegExp a))
  | AnyOf (NonEmpty (RegExp a))
  | OneOrMore (RegExp a)
  | ZeroOrOne (RegExp a)
  | Exactly a
  deriving (Functor, Foldable, Traversable, Show)

type Parser = P.Parsec Void Text

severalOfP :: Parser a -> Parser (RegExp a)
severalOfP p = P.single '(' *> (SeveralOf <$> NE.some1 (P.try $ regExpP p)) <* P.single ')'

anyOfP :: Parser a -> Parser (RegExp a)
anyOfP p = P.single '[' *> (AnyOf <$> NE.some1 (P.try $ regExpP p)) <* P.single ']'

oneOrMoreP :: Parser a -> Parser (RegExp a)
oneOrMoreP p = P.try $ OneOrMore <$> notASuffixP p <* P.single '+'

zeroOrOneP :: Parser a -> Parser (RegExp a)
zeroOrOneP p = P.try $ ZeroOrOne <$> notASuffixP p <* P.single '?'

zeroOrMoreP :: Parser a -> Parser (RegExp a)
zeroOrMoreP p = P.try $ ZeroOrOne . OneOrMore <$> notASuffixP p <* P.single '*'

exactlyP :: Parser a -> Parser (RegExp a)
exactlyP p = Exactly <$> p <?> "a regexp element"

notASuffixP :: Parser a -> Parser (RegExp a)
notASuffixP p =
  severalOfP p
    <|> anyOfP p
    <|> exactlyP p

regExpP :: Parser a -> Parser (RegExp a)
regExpP p =
  oneOrMoreP p
    <|> zeroOrOneP p
    <|> zeroOrMoreP p
    <|> severalOfP p
    <|> anyOfP p
    <|> exactlyP p

parseRegExp :: Text -> Either Text [RegExp Char]
parseRegExp t = first (T.pack . P.errorBundlePretty) $ P.runParser ((many . P.try $ regExpP charParser) <* P.eof) "regexp" t
  where
    charParser = P.noneOf ['[', ']', '(', ')', '+', '?', '*']