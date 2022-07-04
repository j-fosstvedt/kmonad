-- |

module K.Shell.Env
  ( atIvk
  , atCfg
  , atAcq

  , runAt

  , module K.Shell.Env.Initial
  )
where

import K.Shell.Env.Initial
import K.Shell.Error

runAt :: UIO m => ContT a m env -> RIO env a -> m a
runAt c r = runContT c $ inRIO r

atIvk :: CanIvk m => ContT r m AtIvkEnv
atIvk = do
  withShellHandler
  i <- withInvoc
  let c = defShellCfg ^. changed i
  l <- withLogging c
  ContT $ \f -> f (AtIvkEnv c l i)

atCfg :: CanCfg e m => ContT r m AtCfgEnv
atCfg = do
  i <- view $ atIvkEnv.invoc
  f <- loadCfgFile =<< view (atIvkEnv.shellCfg.cfgPath)
  let c = defShellCfg ^. changed f . changed i
  l <- withLogging c
  ContT $ \f -> f (AtCfgEnv c l)

atAcq :: CanAcq e m => ContT r m AtAcqEnv
atAcq = do
  c <- view $ atCfgEnv . shellCfg
  l <- view $ atCfgEnv . logEnv
  k <- withKio (c^.kioCfg)
  ContT $ \f -> f (AtAcqEnv c l k)





-- withShellEnvT :: UIO m => ShellCfg -> ContT r m ShellEnv
-- withShellEnvT cfg = ContT $ withShellEnv cfg

-- -- | Initialize a 'ShellEnv' from a 'ShellCfg' and call a function on it.
-- withShellEnv :: UIO m => ShellCfg -> (ShellEnv -> m a) -> m a
-- withShellEnv c f = withLogging c $ f . ShellEnv c

-- -- | Initialize a 'ShellEnvIO' from a 'ShellEnv' and call a function on it.
-- withShellEnvIO :: (CanLog e m, UIO m) =>  ShellEnv -> (ShellEnvIO -> m a) -> m a
-- withShellEnvIO e f = withKio e $ f . ShellEnvIO e

-- -- | Run a 'Shell' action in IO
-- runShell :: ShellCfg -> Shell a -> IO a
-- runShell cfg = handleShellError . withShellEnv cfg . inRIO

-- -- | flipped 'runShell', mirrors runRIO/inRIO pattern
-- inShell :: Shell a -> ShellCfg -> IO a
-- inShell = flip runShell

-- -- | Run a 'ShellKIO' action in 'Shell'
-- runShellKIO :: ShellKIO a -> Shell a
-- runShellKIO go = ask >>= (`withShellEnvIO` inRIO go)
