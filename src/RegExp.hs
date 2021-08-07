module RegExp where

import Control.Applicative
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Data.Bifunctor (first)
import Data.Functor
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Internal.Fusion.Common as P
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

severalOf :: NonEmpty (RegExp a) -> RegExp a
severalOf (a :| []) = a
severalOf as = SeveralOf as

anyOf :: NonEmpty (RegExp a) -> RegExp a
anyOf (a :| []) = a
anyOf as = AnyOf as

sepBy1 :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepBy1 p sep = (:|) <$> p <*> many (sep *> p)

severalOfP :: Parser a -> Parser (RegExp a)
severalOfP p = P.single '(' *> listParser <* P.single ')'
  where
    exprParser = P.notFollowedBy (P.single ')' <|> P.single '|') *> regExpP p
    subListParser = severalOf <$> NE.some1 exprParser
    listParser = anyOf <$> sepBy1 subListParser (P.single '|')

anyOfP :: Parser a -> Parser (RegExp a)
anyOfP p = P.single '[' *> (anyOf <$> NE.some1 (P.notFollowedBy (P.single ']') *> regExpP p)) <* P.single ']'

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

parseRegExp :: Parser a -> Text -> Either Text (RegExp a)
parseRegExp parser text =
  let sequenceP = severalOf <$> NE.some1 (regExpP parser) <* P.eof
   in first (T.pack . P.errorBundlePretty) $ P.runParser sequenceP "regexp" text

parseCharRegExp :: Text -> Either Text (RegExp Char)
parseCharRegExp = parseRegExp (P.anySingle <?> "character")

parseBraceRegExp :: Text -> Either Text (RegExp Text)
parseBraceRegExp = parseRegExp (P.single '{' *> P.takeWhileP Nothing (/= '}') <* P.single '}' <?> "subexpression")