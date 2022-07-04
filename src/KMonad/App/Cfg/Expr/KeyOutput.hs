-- |

module KMonad.App.Cfg.Expr.KeyOutput where

import KMonad.Prelude

import KMonad.Prelude.Parsing
import KMonad.App.Cfg.Types
import KMonad.App.Cfg.Expr.Cmd
import KMonad.App.Cfg.Expr.Path
import KMonad.App.Cfg.Expr.Types

-- basic types -----------------------------------------------------------------

type KeyOutputExpr = Text

-- | Things that can go wrong with 'KeyOutputCfg' resolution
newtype KeyOutputExprError = KeyOutputParseError ParseError deriving Eq
makeClassyPrisms ''KeyOutputExprError

instance Show KeyOutputExprError where
  show (KeyOutputParseError e) =
    "Parse error in KeyOutput expression:\n" <> show e

instance Exception KeyOutputExprError
instance AsKeyOutputExprError SomeException where _KeyOutputExprError = exception

-- basic ops -------------------------------------------------------------------

-- | An 'Expr' expressing 'KeyOutputCfg'
keyOutputExpr :: Expr KeyOutputCfg
keyOutputExpr = Expr outputT (parse outputP) _KeyOutputParseError

-- | An 'Iso' between 'KeyOutputCfg' and 'KeyOutputExpr'
_KeyOutputExpr :: Iso' KeyOutputCfg KeyOutputExpr
_KeyOutputExpr = exprIso keyOutputExpr

-- exprs -----------------------------------------------------------------------

-- | A parser that tries to extract an 'OutputCfg' from 'Text'
--
-- Patterns
-- - lin:uinput:
-- - lin:uinput:My keyboard name
-- - win:send:
-- - mac:kext:
-- - cmdsnk:cmd:evtest this thing for me
-- - cmdsnk:exec:evtest:["this", "thing", "for", "me"]
-- - stdout:
outputP :: Parser KeyOutputCfg
outputP = choice
  [ string "lin:uinput:" *> (LinUinputSnk <$> maybeRestP)
  , string "win:send:" $> WinSendSnk
  , string "mac:kext:" $> MacKextSink
  , string "cmdsnk:" *> (CmdSnk <$> cmdP)
  , string "stdout:" $> StdoutSnk
  ]

-- | Create the textual representation of a 'KeyOutputCfg'
outputT :: KeyOutputCfg -> KeyOutputExpr
outputT (LinUinputSnk mt) = "lin:uinput:" <> fromMaybe "" mt
outputT WinSendSnk = "win:send:"
outputT MacKextSink = "mac:kext:"
outputT (CmdSnk c) = "cmdsnk:" <> c^._CmdExpr
outputT StdoutSnk = "stdout:"
