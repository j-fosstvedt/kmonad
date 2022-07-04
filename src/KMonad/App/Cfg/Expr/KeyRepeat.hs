-- |

module KMonad.App.Cfg.Expr.KeyRepeat where

import KMonad.Prelude
import KMonad.Prelude.Parsing

import KMonad.Keyboard
import KMonad.App.Cfg.Types
import KMonad.App.Cfg.Expr.Types

-- basic types -----------------------------------------------------------------

type KeyRepeatExpr = Text

-- | Things that can go wrong with 'KeyRepeat' resolution
newtype KeyRepeatExprError = KeyRepeatParseError ParseError deriving Eq
makeClassyPrisms ''KeyRepeatExprError

instance Show KeyRepeatExprError where
  show (KeyRepeatParseError e) =
    "Parse error in KeyRepeat expression:\n" <> show e

instance Exception KeyRepeatExprError
instance AsKeyRepeatExprError SomeException where _KeyRepeatExprError = exception

-- basic ops -------------------------------------------------------------------

keyRepeatExpr :: Expr KeyRepeatCfg
keyRepeatExpr = Expr keyRepeatT (parse keyRepeatP) _KeyRepeatParseError

_KeyRepeatExpr :: Iso' KeyRepeatCfg KeyRepeatExpr
_KeyRepeatExpr = exprIso keyRepeatExpr

-- | A parser that tries to extract a 'KeyRepeatCfg' from 'Text'
--
-- Patterns
-- - sim:300:100
keyRepeatP :: Parser KeyRepeatCfg
keyRepeatP = choice
  [ string "sim:" *> (Simulate <$> (DelayRate <$> (msP <* char ':') <*> msP))
  , string "echo:" $> EchoOS
  , string "ignore:" $> IgnoreRepeat
  ]

-- | Create the textual representation of a 'KeyRepeatCfg'
keyRepeatT :: KeyRepeatCfg -> KeyRepeatSpec
keyRepeatT (Simulate (DelayRate d r)) = "sim:" <> tshow (d^.ms) <> ":" <> tshow (r^.ms)
keyRepeatT EchoOS = "echo:"
keyRepeatT IgnoreRepeat = "ignore:"
