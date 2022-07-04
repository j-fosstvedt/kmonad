-- |

module KMonad.App.Cfg.Default where

import KMonad.Prelude

import KMonad.App.Cfg.Types
import KMonad.App.Cfg.Expr

import qualified RIO.HashMap as M


defAppCfg :: AppCfg
defAppCfg = AppCfg
  { _appLocaleCfg = LocaleCfg
    { _namedCodes    = M.fromList []
    , _namedGestures = M.fromList []
    }
  , _appLogCfg = LogCfg
    { _logLevel = LevelWarn
    }
  , _appKioCfg = KioCfg
    { _keyRepeatCfg = IgnoreRepeat
    , _fallthrough = False
    , _keyInputCfg = StdinSrc
    , _keyOutputCfg = StdoutSnk
    , _preKioCmd = NoCmd
    , _postKioCmd = NoCmd
    }
  , _appRunCfg = RunCfg
    { _cfgPath = "xdgcfg:kmonad.dhall" ^. from _PathExpr
    , _kbdPath = "xdgcfg:keymap.kbd" ^. from _PathExpr
    , _cmdAllow = NoCmds
    , _runType = FullRun
    }
  }


type ShellFlag = Flag ShellCfg
type ShellOption = Option ExprError ShellCfg

shellFlags :: [Flag ShellCfg]
shellFlags =
  [ mkFlag "fallthrough" (Just 'T')
      "Enable unaltered reemission of uncaught events."
      $ setVal fallthrough True "set fallthrough to True"

  , mkFlag "log-to-stderr" (Just 'E')
      "Write log-messages to stderr instead of stdout"
      $ setVal logTarget stderr "set logTarget to stderr"

  , invocOnly $ mkFlag "verbose" (Just 'v')
      "Make logging very verbose"
      $ setVal logLevel LevelDebug "set logLevel to LevelDebug"
  ]

shellOptions :: [Option ExprError ShellCfg]
shellOptions =
  [ -- RunCfg ------------------------------------------------------------------

    invocOnly $ mkOption "run-type" (Just 'r')
      "Task to execute: run | test | discover"
      $ setWithExpr task taskExpr "task"

  , invocOnly $ mkOption "cfg-path" (Just 'f')
      "Path to dhall-syntax configuration file"
      $ setWithExpr cfgPath pathExpr "cfgPath"

  , mkOption "kbd-path" (Just 'k')
      "Path to klang-syntax keymap file"
      $ setWithExpr kbdPath pathExpr "kbdPath"

  , mkOption "cmd-allow" (Just 'c')
      "What commands to allow: none | init | all"
      $ setWithExpr cmdAllow cmdAllowExpr "cmdAllow"

    -- LogCfg ------------------------------------------------------------------

  , mkOption "log-level" (Just 'l')
      "Minimum urgency that gets displayed: error | warn | info | debug"
      $ setWithExpr logLevel logLevelExpr "logLevel"

  , mkOption "log-color" (Just 'C')
      "Coloration strategy for logging output: dark-bg | light-bg | none"
      $ setWithExpr logColor logColorExpr "logColor"

    -- KioCfg ------------------------------------------------------------------

  , mkOption "key-input" (Just 'i')
      "Input expression defining how to acquire keyboard"
      $ setWithExpr keyInputCfg keyInputExpr "keyInputCfg"

  , mkOption "key-output" (Just 'o')
      "Output expression defining how to simulate keyboard"
      $ setWithExpr keyOutputCfg keyOutputExpr "keyOutputCfg"

  , mkOption "pre-kio-cmd" (Just 'b')
      "Command expression to be run before acquiring key-IO"
      $ setWithExpr preKioCmd cmdExpr "preKioCmd"

  , mkOption "post-kio-cmd" (Just 'a')
      "Command expression to be run before acquiring key-IO"
      $ setWithExpr postKioCmd cmdExpr "postKioCmd"

  , mkOption "key-repeat" (Just 'e')
      "KeyRepeat expression describing how to handle key-repeat"
      $ setWithExpr keyRepeatCfg keyRepeatExpr "keyRepeatCfg"
  ]
