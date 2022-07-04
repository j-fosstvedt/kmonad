-- |

module KMonad.App.Cfg.Expr.Logging where

import KMonad.Prelude
import KMonad.Prelude.Parsing
import KMonad.App.Cfg.Types

import RIO.Text as T

-- | Try to parse a 'LogLevel' from 'Text'
logLevelP :: Parser LogLevel
logLevelP = choice
  [ string "debug" $> LevelDebug
  , string "info"  $> LevelInfo
  , string "warn"  $> LevelWarn
  , string "error" $> LevelError
  ]

-- | Create the textual representatino of a 'LogLevel'
logLevelS :: LogLevel -> LogLevelSpec
logLevelS = T.drop 5 . T.toLower . tshow
