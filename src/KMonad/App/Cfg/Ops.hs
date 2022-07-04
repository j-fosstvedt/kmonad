{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |

module KMonad.App.Cfg.Ops where

import KMonad.Prelude
import KMonad.App.Cfg.Types
import KMonad.App.Cfg.Expr.Path
-- import KMonad.App.Cfg.Spec

import KMonad.Prelude.Parsing
import KMonad.Gesture
-- import KMonad.Keyboard


import Control.Monad.Except
import Control.Exception.Lens

import qualified RIO.HashMap as M
import qualified RIO.Text as T
import qualified RIO.List as L
import qualified Dhall as D

loadAppCfg :: MonadIO m => Invoc -> m AppCfg
loadAppCfg = undefined

-- | Try to load a 'DhallCfg' from disk
loadDhallCfg :: MonadIO m => Maybe FileSpec -> m DhallCfg
loadDhallCfg mf = do
  pth <- resolve =<< case readPath (fromMaybe defCfgFile mf) of
    Left e -> throwIO e
    Right x -> pure x
  let opt = D.defaultInterpretOptions { D.fieldModifier = T.drop 2 }
  let dec = D.genericAutoWith opt
  liftIO $ D.inputFile dec pth

--------------------------------------------------------------------------------

type Keycode = Natural


--------------------------------------------------------------------------------

data CfgEnv = CfgEnv
  { _dfg :: DhallCfg
  , _ivk :: Invoc
  }
makeClassy ''CfgEnv

instance HasDhallCfg CfgEnv where dhallCfg = dfg
instance HasInvoc CfgEnv where invoc = ivk

-- | A simple monad with read access to input and CfgError exceptions
type C a = ReaderT CfgEnv (Except CfgError) a

-- | How to run the C monad
runC :: DhallCfg -> Invoc -> C a -> Either CfgError a
runC d i = runExcept . (`runReaderT` CfgEnv d i)

-- | Get the value for a setting in the C monad
getS :: Getter DhallCfg a -> Getter Invoc (Maybe a) -> C a
getS d i = view (cfgEnv.invoc.i) >>= \case
  Just x  -> pure x
  Nothing -> view $ cfgEnv.dhallCfg.d

--------------------------------------------------------------------------------



data CfgError
  = CfgLocaleError LocaleError
  | CfgPathError   ParseError

-- | Take an 'Invoc' and a 'DhallCfg' and construct the 'AppCfg'
--
-- First, we take the dhall config (KMonad defaults are encoded in the dhall
-- configuration code) and we construct a valid AppCfg. Then we override any
-- settings provided in the invoc.
buildCfg :: C AppCfg
buildCfg = do

  -- Validate and construct the locale
  loc <- left CfgLocaleError $
    mkLocale (d^.dcodeNames.from _DMap) (d^.dgestureNames.from _DMap)

  -- Validate and construct a bunch of settings
  keymapPath <- left CfgPathError $ readPath (getS dkeymapFile ikeymapFile)
  cmdAllow <- getS dcmdAllow icmdAllow




  Right $ undefined

--------------------------------------------------------------------------------

foo :: [Either LocaleError LocaleCfg]
foo = map (uncurry mkLocale)
  [ ([("a", 1), ("b", 2), ("S", 3)], [("party", "a b a b" ), ("joy", "S-[a b]")])
  , ([("a", 1), ("a", 2)], [])
  , ([], [("a", "b"), ("a", "c")])
  , ([("a", 1)], [("a", "l")])
  ]

fooM :: IO ()
fooM = pPrint foo
