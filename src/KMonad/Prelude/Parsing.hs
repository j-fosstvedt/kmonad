-- | A collection of general parsing definitions

module KMonad.Prelude.Parsing
  ( Parser
  , ParserT
  , ParseError(..)
  , AsParseError(..)

  , parse
  , parseT

  , sc
  , hsc
  , lex
  , hlex

  , msP
  , natP
  , textP
  , listOfP

  , maybeRestP
  , namedP

  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  )

where

import KMonad.Prelude hiding (try)
import Control.Arrow
import Text.Megaparsec hiding (ParseError, parse)
import Text.Megaparsec.Char

import qualified Text.Megaparsec.Char.Lexer as X
import qualified RIO.List as L
import qualified RIO.Text as T

--------------------------------------------------------------------------------

-- | Parsec type specified down to Void Text
type Parser a    = Parsec Void Text a
type ParserT m a = ParsecT Void Text m a

-- | Parsec parse errors under Void Text with an Exception instance
newtype ParseError = ParseError { _parseError :: ParseErrorBundle Text Void}
  deriving Eq
makeClassyPrisms ''ParseError

instance Show ParseError where
  show (ParseError e) = "Parse error at " <> errorBundlePretty e

instance Exception ParseError


--------------------------------------------------------------------------------

parse :: Parser a -> Text -> Either ParseError a
parse p = runIdentity . parseT p

parseT :: Monad m => ParserT m a -> Text -> m (Either ParseError a)
parseT p = fmap (left ParseError) . runParserT p ""

--------------------------------------------------------------------------------

-- | Horizontal space consumption
hsc :: Parser ()
hsc = X.space space1 empty empty

-- | Horizontal space lexeme
hlex :: Parser a -> Parser a
hlex = X.lexeme hsc

-- | Full space consumption
sc :: Parser ()
sc = X.space space1 (X.skipLineComment  ";;") (X.skipBlockComment "#|" "|#")

-- | Full space lexeme
lex :: Parser a -> Parser a
lex = X.lexeme sc

--------------------------------------------------------------------------------

-- | Parse a non-negative integer
natP :: Parser Natural
natP = X.decimal <?> "natural number"

-- | Parse a natural number as a 'Dt' expressed as in milliseconds
msP :: Parser Dt
msP = view (from ms) <$> natP <?> "natural number expressing ms"

-- | Parse a ""-surrounded string, supporting @\@-style escapes
textP :: Parser Text
textP = pack <$> (char '\"' *> manyTill X.charLiteral (char '\"' ))

-- | Parse a []-surrounded string with ,-separated values
listOfP :: Parser a -> Parser [a]
listOfP p = between (char '[') (char ']') $ sepBy p (hlex $ char ',')

--------------------------------------------------------------------------------

-- | Return 'Nothing' if at 'eof', otherwise 'Just' the rest of the text
maybeRestP :: Parser (Maybe Text)
maybeRestP = Nothing <$ eof <|> Just <$> takeRest

-- | Create a parser that matches symbols to values and only consumes on match.
namedP :: Named a -> Parser a
namedP = do
  -- Sort descending in length, and then alphabetically
  let srt = L.sortBy . (`on` fst) $ \a b ->
        case compare (T.length b) (T.length a) of
          EQ -> compare a b
          x  -> x
  choice . map (\(s, x) -> try (string s) $> x). srt
