-- |

module KMonad.App.Cfg.Expr.Cmd where


import KMonad.Prelude
import KMonad.Prelude.Parsing

import KMonad.App.Cfg.Types
import KMonad.App.Cfg.Expr.Types

-- basic types -----------------------------------------------------------------

type CmdExpr = Text

-- | Things that can go wrong with 'Cmd' resolution
newtype CmdExprError = CmdParseError ParseError deriving Eq
makeClassyPrisms ''CmdExprError

instance Show CmdExprError where
  show (CmdParseError e) =
    "Parse error in Cmd expression:\n" <> show e

instance Exception CmdExprError
instance AsCmdExprError SomeException where _CmdExprError = exception

-- basic ops -------------------------------------------------------------------

cmdExpr :: Expr Cmd
cmdExpr = Expr cmdT (parse cmdP) _CmdParseError

-- | An Iso between 'Text' and 'Cmd' values
_CmdExpr :: Iso' Cmd CmdExpr
_CmdExpr = exprIso cmdExpr

-- exprs -----------------------------------------------------------------------

{- NOTE: The basic syntax for cmds is:
- a preamble containing either 'exec:' or 'cmd:'
- if 'cmd:' -> The rest of the string is a shell command
- if 'exec:' -> 'name-of-command':, then list of command-separated string args

"cmd:echo foo > /tmp/example.txt"
"exec:rm:["/tmp/*", "-r", "-f"]" (in haskell, you'd have to escape the "-marks)

no shorthand at the moment

-}

-- | Create the textual representation of a 'Cmd'
cmdT :: Cmd -> CmdExpr
cmdT (SimpleCmd t) = "cmd:" <> t
cmdT (CompoundCmd e a) = "exec:" <> pack e <> ":" <> tshow a
cmdT NoCmd = "pass"

-- | A parser that tries to extract a 'Cmd' from 'Text'
--
-- Patterns
-- - "cmd:evtest this thing for me"
-- - "exec:rm:["/*", "-rf"]"
cmdP :: Parser Cmd
cmdP = choice
  [ string "cmd:" *> (SimpleCmd <$> takeRest)
  , string "exec:" *> (CompoundCmd <$> exe <*> args)
  , string "pass:" $> NoCmd
  ]
  where
    exe = unpack <$> takeWhile1P Nothing (/= ':') <* char ':'
    args = listOfP textP
