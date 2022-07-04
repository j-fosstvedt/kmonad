-- |

module KMonad.App.Cfg.Expr.Types where

import KMonad.Prelude
import KMonad.Prelude.Parsing

import KMonad.App.Cfg.Types

-- expr ------------------------------------------------------------------------

-- | An 'Expr' describing the relationship between 'Text' and some value
data Expr e a = Expr
  { _toText :: a -> Text                          -- ^ How to encode a value to text
  , _fromText :: Text -> Either ParseError a      -- ^ How to decode a value from text
  , _errPrism :: AReview e ParseError -- ^ How to render a decoding error
  }
makeLenses ''Expr

encode :: Expr e a -> a -> Text
encode = view toText

decode :: MonadError e m => Expr e a -> Text -> m a
decode e t = throwEither (e^.errPrism) (e^.fromText $ t)

-- named-expr types ------------------------------------------------------------

newtype NamedExprError = NamedParseError ParseError deriving Eq
makeClassyPrisms ''NamedExprError

instance Show NamedExprError where
  show (NamedParseError e) =
    "Parse error in Named expression:\n" <> show e

instance Exception NamedExprError
-- instance AsNamedExprError SomeException where _NamedExprError = exception

namedExpr :: (AsNamedExprError e, Eq a) => Named a -> Expr e a
namedExpr ns = Expr nameFor' (parse $ namedP ns) _NamedParseError
  where
    nameFor' a = fromMaybe (error msg) $ nameFor a ns
    msg = "Programmer did not specify names for every option. Please complain on github."

-- named-expr vals -------------------------------------------------------------

boolExpr :: (AsNamedExprError e) => Expr e Bool
boolExpr = namedExpr [("off", False), ("on", True)]

runTypeExpr :: (AsNamedExprError e) => Expr e RunType
runTypeExpr = namedExpr [("run", FullRun), ("test", CfgTest), ("discover", EvTest)]

logLevelExpr :: (AsNamedExprError e) => Expr e LogLevel
logLevelExpr = namedExpr [ ("error", LevelError), ("warn", LevelWarn)
                         , ("info", LevelInfo), ("debug", LevelDebug) ]

cmdAllowExpr :: (AsNamedExprError e) => Expr e CmdAllow
cmdAllowExpr = namedExpr [("none", NoCmds), ("init", InitCmds), ("all", AllCmds)]
