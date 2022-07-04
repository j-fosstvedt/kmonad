-- |

module K.Shell.Main

where

import K.Initial
import K.Shell.Cfg
import K.Shell.Env
import K.Shell.Error
import K.Shell.Logging
import K.Shell.KeyIO

import K.Layers

import qualified Control.Exception.Lens as Exc



-- | Start KMonad.
begin :: IO ()
begin = runAt atIvk doTask

-- | Choose what to do based on the 'task' setting in the 'ShellCfg'
doTask :: AtIvk ()
doTask = view task >>= \case
  -- Run any task that does not need anything more than an invocation
  SendMsg -> devFail "not implemented yet"
  x -> runAt atCfg $ case x of
    -- Run any task that does not need KeyIO
    CfgTest -> cfgTest
    y -> runAt atAcq $ case y of
      -- Run any task that needs all shell capacities
      FullRun -> loop
      EvTest  -> discover

--   -- withShellCfg $ inShell doTask

-- loadKeymap :: Shell ()
-- loadKeymap = do
--   p <- throwLeft $ "x:test.kbd" ^. decoded pathExpr
--   void $ pPrint =<< loadKbdFile p


-- | The standard 'do the remapping' task
loop :: AtAcq ()
loop = do
  forever $ do
    e <- waitKioEvent
    atInfo $ pp e
    sendKioEvent e

-- | Print information about all events that arrive at kmonad
discover :: AtAcq ()
discover = do
  logError "behold"

cfgTest :: AtCfg ()
cfgTest = do
  logError "boheld"
