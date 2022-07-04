-- |

module K.Shell.Env.Initial
  ( AtIvkEnv(..)
  , AtCfgEnv(..)
  , AtAcqEnv(..)

  , HasAtIvkEnv(..)
  , HasAtCfgEnv(..)

  , AtIvk
  , AtCfg
  , AtAcq

  , CanIvk
  , CanCfg
  , CanAcq

  , module K.Shell.Initial
  , module K.Shell.Cfg
  , module K.Shell.Logging
  , module K.Shell.KeyIO
  )
where

import K.Shell.Initial
import K.Shell.Cfg
import K.Shell.Logging
import K.Shell.KeyIO

import Control.Monad.Catch

-- runtime environment ---------------------------------------------------------

data AtIvkEnv = AtIvkEnv
  { _iShellCfg :: ShellCfg
  , _iLogEnv   :: LogEnv
  , _invoc    :: Invoc
  }
makeClassy ''AtIvkEnv

instance HasLogEnv AtIvkEnv where logEnv = iLogEnv
instance HasShellCfg AtIvkEnv where shellCfg = iShellCfg
instance HasRunCfg AtIvkEnv where runCfg = shellCfg.runCfg

data AtCfgEnv = AtCfgEnv
  { _cShellCfg :: ShellCfg
  , _cLogEnv   :: LogEnv
  }
makeClassy ''AtCfgEnv

instance HasLogEnv AtCfgEnv where logEnv = cLogEnv
instance HasShellCfg AtCfgEnv where shellCfg = cShellCfg

data AtAcqEnv = AtAcqEnv
  { _aShellCfg :: ShellCfg
  , _aLogEnv   :: LogEnv
  , _aKioEnv   :: KioEnv
  }
makeClassy ''AtAcqEnv

instance HasLogEnv AtAcqEnv where logEnv = aLogEnv
instance HasKioEnv AtAcqEnv where kioEnv = aKioEnv
instance HasShellCfg AtAcqEnv where shellCfg = aShellCfg

-- shorthand -------------------------------------------------------------------

-- | The different capacity levels the shell monad can run at

-- | Lowest capacity level
-- 1. access to cfg defaults overwritten by invoc
-- 2. running in a top-level exception handler
-- 3. access to a logging environment
type AtIvk a = RIO AtIvkEnv a

-- | Medium capacity level
-- 4. access to cfg default overwritten by cfgfile overwritten by invoc
-- 5. since locale is cfg-file-only, locale is now available
type AtCfg a = RIO AtCfgEnv a

-- | Full capacity
-- 6. access to an acquired keysource, sink, and repeat environment
type AtAcq a = RIO AtAcqEnv a

type CanIvk m = (UIO m, MonadCatch m)

type CanCfg e m = (CanIvk m, MonadReader e m, HasAtIvkEnv e, HasLogEnv e)

type CanAcq e m = (CanIvk m, MonadReader e m, HasAtCfgEnv e, HasLogEnv e)
