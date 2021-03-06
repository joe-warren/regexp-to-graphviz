module RegExp where

import Control.Applicative
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
    exprParser = P.notFollowedBy (P.single ')' <|> P.single '|') *> singleRegExpP p
    subListParser = severalOf <$> NE.some1 exprParser
    listParser = anyOf <$> sepBy1 subListParser (P.single '|')

anyOfP :: Parser a -> Parser (RegExp a)
anyOfP p = P.single '[' *> (anyOf <$> NE.some1 (P.notFollowedBy (P.single ']') *> singleRegExpP p)) <* P.single ']'

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

singleRegExpP :: Parser a -> Parser (RegExp a)
singleRegExpP p = (withoutSuffixP p <**> suffixP)

topLevelSeveralOfP :: Parser (RegExp a) -> Parser (RegExp a)
topLevelSeveralOfP = fmap severalOf . NE.some1

regExpP :: Parser a -> Parser (RegExp a)
regExpP p = topLevelSeveralOfP (singleRegExpP p)

parseRegExp :: Parser a -> Text -> Either Text (RegExp a)
parseRegExp parser text = first (T.pack . P.errorBundlePretty) $ P.runParser (regExpP parser) "regexp" text

charRegExpParser :: Parser Char
charRegExpParser = (P.anySingle <?> "character")

parseCharRegExp :: Text -> Either Text (RegExp Char)
parseCharRegExp = parseRegExp charRegExpParser

braceRegExParser :: Parser Text
braceRegExParser = (P.single '{' *> P.takeWhileP Nothing (/= '}') <* P.single '}' <?> "subexpression")

nestedParser :: Parser a -> Parser (RegExp a)
nestedParser child = (P.single '{') *> (regExpP (P.notFollowedBy (P.single '}') *> child)) <* (P.single '}')

parseBraceRegExp :: Text -> Either Text (RegExp Text)
parseBraceRegExp = parseRegExp braceRegExParser