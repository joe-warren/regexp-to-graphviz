module RegExp where

import Control.Applicative
import Data.Bifunctor (first)
import Data.Functor
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (Traversable)
import Data.Void (Void)
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Byte as P

data RegExp a
  = SeveralOf (NonEmpty (RegExp a))
  | AnyOf (NonEmpty (RegExp a))
  | OneOrMore (RegExp a)
  | ZeroOrOne (RegExp a)
  | Exactly a
  deriving (Functor, Foldable, Traversable, Show)

type Parser = P.Parsec Void Text

severalOfP :: Parser a -> Parser (RegExp a)
severalOfP p = P.single '(' *> (SeveralOf <$> NE.some1 (P.notFollowedBy (P.single ')') *> regExpP p)) <* P.single ')'

anyOfP :: Parser a -> Parser (RegExp a)
anyOfP p = P.single '[' *> (AnyOf <$> NE.some1 (P.notFollowedBy (P.single ']') *> regExpP p)) <* P.single ']'

exactlyP :: Parser a -> Parser (RegExp a)
exactlyP p = Exactly <$> p

suffixP :: Parser (RegExp a -> RegExp a)
suffixP =
  P.single '+' $> OneOrMore
    <|> P.single '?' $> ZeroOrOne
    <|> P.single '*' $> (ZeroOrOne . OneOrMore)
    <|> pure id

withoutSuffixP :: Parser a -> Parser (RegExp a)
withoutSuffixP p =
  severalOfP p
    <|> anyOfP p
    <|> exactlyP p

regExpP :: Parser a -> Parser (RegExp a)
regExpP p = withoutSuffixP p <**> suffixP

parseRegExp :: Text -> Either Text [RegExp Char]
parseRegExp t = first (T.pack . P.errorBundlePretty) $ P.runParser ((P.many $ regExpP charParser) <* P.eof) "regexp" t
  where
    charParser = P.anySingle <?> "elem" --P.noneOf ['[', ']', '(', ')', '+', '?', '*']