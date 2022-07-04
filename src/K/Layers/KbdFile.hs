-- |

module K.Layers.KbdFile where

import K.Initial.Parsing
import K.Layers.Initial
import K.Keyboard

-- TEMP
import K.Shell.Cfg

import Data.Char

import qualified Control.Exception.Lens as Exc
import qualified RIO.HashMap as M
import qualified RIO.Text as T

-- error -----------------------------------------------------------------------

data KbdError
  = KbdParseError ParseError
  | KbdLocaleError LocaleError
makeClassyPrisms ''KbdError

instance Show KbdError where
  show (KbdParseError e)
    = "Parse error in kbd-file: " <> show e
  show (KbdLocaleError e)
    = "Loclae error in kbd-file: " <> show e

instance Exception KbdError
instance AsKbdError SomeException where _KbdError = _SomeException
instance AsParseError KbdError where __ParseError = _KbdParseError
instance AsLocaleError KbdError where _LocaleError = _KbdLocaleError

-- ops -------------------------------------------------------------------------

loadKbdFile :: (MonadReader e m, HasLocale e, MonadIO m)
  => Path -> m [KExpr Keycode] -- KeymapCfg
loadKbdFile p = do
  txt <- readFileUtf8 =<< resolve p

  parseT (some kexprP) txt >>= \case
    Left e -> Exc.throwing _KbdParseError e
    Right xs -> pure xs -- validateKeymap xs -- pure x


foo :: Path
foo = Path "test.kbd" (Just XdgCfg) False

loc :: IO Locale
loc = do
  f <- loadCfgFile $ Path "test.dhall" (Just XdgCfg) False
  pure $ defShellCfg ^. changed f . locale

-- basic types -----------------------------------------------------------------

-- | All the different klang top-level expressions
data KExpr c
  = KSrc SrcName [c]                    -- ^ A `defsrc` expression
  | KLayer LayerName [But c]            -- ^ A `deflayer` expression
  | KPairs LayerName [(Keyname, But c)] -- ^ A `defpairs` expression
  | KAlias [(ButtonName, But c)]        -- ^ A `defalias` expression
  deriving (Eq, Show, Functor)

type Loc e m = (MonadReader e m, HasLocale e)

-- top-level expressions -------------------------------------------------------

kexprP :: Loc e m => ParserT m (KExpr Keycode)
kexprP = choice . map try $ [srcP, layerP, aliasP] -- pairsP

srcP :: Loc e m => ParserT m (KExpr Keycode)
srcP = statement "defsrc" $ do
  n <- fromMaybe "" <$> lex (optional $ keywordP "name" nameP)
  c <- some (lex keycodeP)
  pure $ KSrc n c

layerP :: Loc e m => ParserT m (KExpr Keycode)
layerP = statement "deflayer" $ do
  n <- lex nameP
  b <- some (lex butP)
  pure $ KLayer n b

-- pairsP :: ParserT m (KExpr Keycode)
-- pairsP = statement "defpairs" $ undefined

aliasP :: Loc e m => ParserT m (KExpr Keycode)
aliasP = statement "defalias" $ do
  b <- some $ (,) <$> lex nameP <*> lex butP
  pure $ KAlias b

-- combinators -----------------------------------------------------------------

-- | Run a parser between 2 sets of parentheses
paren :: ParserT m a -> ParserT m a
paren = between (symbol "(") (symbol ")")

-- | Run a parser between 2 sets of parentheses starting with a symbol
statement :: Text -> ParserT m a -> ParserT m a
statement s = lex . paren . (symbol s *>)

-- | Parse a LISP-like keyword of the form @:keyword value@
keywordP :: Text -> ParserT m a -> ParserT m a
keywordP kw p = symbol (":" <> kw) *> lex p
  <?> "Keyword: " <> T.unpack kw

-- simple expr -----------------------------------------------------------------

-- | List of all characters that /end/ a word or sequence
terminators :: String
terminators = ")\""

-- | Parse valid characters until white-space or a terminator
nameP :: ParserT m Text
nameP = T.pack <$> some (satisfy wordChar)
  where wordChar c = not (isSpace c || c `elem` terminators)

-- | Parse a 'Keycode' by parsing a name from 'namedCodes' in the 'Locale'
keycodeP :: (Loc e m) => ParserT m Keycode
keycodeP = namedP . M.toList =<< view namedCodes

-- | Parse a 'Rap' by parsing a name from 'namedRaps' in the 'Locale'
rapP :: (Loc e m) => ParserT m Rap
rapP = namedP . M.toList =<< view namedRaps

-- buttons ---------------------------------------------------------------------

butP :: (Loc e m) => ParserT m (But Keycode)
butP = simpleBP

-- | Parse a simple, non-keyword button
simpleBP :: (Loc e m) => ParserT m (But Keycode)
simpleBP = choice
  [ BEmit <$> keycodeP
  ]
