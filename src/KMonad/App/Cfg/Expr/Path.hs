-- |

module KMonad.App.Cfg.Expr.Path where

import KMonad.Prelude hiding (try)

import KMonad.Prelude.Parsing
import KMonad.App.Cfg.Types
import KMonad.App.Cfg.Expr.Types

import System.FilePath.Glob (glob)
import System.Info (os)
import UnliftIO.Directory
import RIO.FilePath ((</>))

import qualified RIO      as S (unlines)
import qualified RIO.Text as T

-- basic types -----------------------------------------------------------------

-- | A type alias for some text that we expect to parse to a valid 'Path'
type PathExpr = Text

-- | Things that can go wrong with 'Path' resolution
data PathExprError
  = PathParseError      ParseError
  | GlobNoMatch         FilePath
  | GlobMultipleMatches FilePath [FilePath]
  deriving Eq
makeClassyPrisms ''PathExprError

instance Show PathExprError where
  show (PathParseError e) =
    "Parse error in Path expression:\n" <> show e
  show (GlobNoMatch t) =
    "Glob \"" <> t <> "\" matches 0 files"
  show (GlobMultipleMatches t fs)  = S.unlines $ [
    "Glob \"" <> t <> "\" matches multiple files:"
    ] <> fs

instance Exception PathExprError
-- instance AsPathExprError SomeException where _PathExprError = exception

-- basic ops -------------------------------------------------------------------

-- | Resolve a 'Path' value to an absolute 'FilePath'
--
-- This may throw a
-- * GlobNoMatch when a glob matches no files
-- * GlobMultipleMatches when a glob matches more than 1 file
--
-- Note that a succesful resolution does not guarantee the file exists, only
-- that during the 'resolve' call the 'Path' made enough sense to create a
-- single 'FilePath'.
resolve :: MonadIO m => Path -> m FilePath
resolve p = do
  r <- (</> (unpack $ p^.val)) <$> case p^.root of
    Nothing         -> pure ""
    Just XdgCfg     -> getXdgDirectory XdgConfig "kmonad"
    Just Home       -> getHomeDirectory
    Just (Custom t) -> pure $ unpack t

  if not $ p^.doGlob then pure r else (liftIO . glob $ r) >>= \case
    [] -> throwIO . GlobNoMatch $ r
    [f] -> pure f
    fs  -> throwIO $ GlobMultipleMatches r fs

-- | An 'Expr Path' with configurable extra directories
pathExprWith :: (AsPathExprError e) => Named PathRoot -> Expr e Path
pathExprWith nr = Expr (pathT nr) (parse (pathP nr)) _PathParseError

-- | An 'Expr Path' with configurable extra directories
pathExpr :: (AsPathExprError e) => Expr e Path
pathExpr = pathExprWith []

-- | An Iso between Text and Path with configurable extra directories
-- _PathExprWith :: Named PathRoot -> Iso' Path Text
-- _PathExprWith nr = exprIso $ pathExprWith nr

-- -- | An Iso between Text and Path
-- _PathExpr ::  Iso' Path Text
-- _PathExpr = _PathExprWith []

-- exprs -----------------------------------------------------------------------

{- NOTE:

"[glob:][root:]rest/of/the/expression.ext"
"[[g][c]:]rest/of/the/expression.ext"

shorthand:
g: glob
x: xdgcfg
h: home
-}

-- | Predefined terms referring to certain standard 'PathRoot's
roots :: Named PathRoot
roots = [("xdgcfg", XdgCfg), ("home", Home)]

-- parsing ---------------------------------------------------------------------

-- | Create a 'Parser' for 'Path's, allowing for additional 'Named PathRoot's.
pathP :: Named PathRoot -> Parser Path
pathP nr = do

  -- Make sure there is no clash between names or characters
  let r = map (over _1 (<> ":")) roots <> nr    -- All ("long-name", Root)''s
  let l = checkNamesThrow $ "glob": (r^..names) -- All long preamble terms, validated
  let _ = checkNamesThrow $ map (T.take 1) l    -- All short preamble terms, validated

  pre <- preambleLong r <|> preambleShort (over (mapped._1) (T.take 1) r)
  rst <- exprTail

  pure $ Path rst (pre^._2) (pre^._1)

-- | Parse a long preamble consisting of full, colon-terminated names
preambleLong  :: Named PathRoot -> Parser (Bool, Maybe PathRoot)
preambleLong nr = do
  g <- optional . try $ string "glob:"
  r <- optional . namedP $ nr
  case (g, r) of (Nothing, Nothing) -> empty
                 _                  -> pure (isJust g, r)

-- | Parse a short preamble consisting of initial characters followed by a colon
preambleShort :: Named PathRoot -> Parser (Bool, Maybe PathRoot)
preambleShort nr = do
  p <- optional . try $ do
    g' <- optional $ char 'g'
    r' <- optional . namedP $ nr
    (g', r') <$ char ':'
  pure . maybe (False, Nothing) (over _1 isJust) $ p

-- | Parse the non-preamble portion of the string, any non-colon or double-colon
exprTail :: Parser Text
exprTail = do
  let nc = takeWhile1P (Just "non-colon character") (/= ':')
  let ec = ":" <$ string "::" <?> ":: (escaped colon)"
  mconcat <$> some (try ec <|> nc) <* (void eol <|> eof)

-- printing --------------------------------------------------------------------

pathT :: Named PathRoot -> Path -> Text
pathT nr p = let
  b = if p^.doGlob then "glob:" else ""
  d = maybe "" (<> ":") $ (`nameFor` (nr <> roots)) =<< p^.root
  in b <> d <> p^.val
