module KMonad.App.Cfg.Types where


import KMonad.Prelude


import KMonad.Gesture
import KMonad.Logging.Cfg
-- import KMonad.App.Cfg.Expr.Cmd
-- import KMonad.App.Cfg.Expr.Path
import KMonad.Keyboard.Types (DelayRate(..))

import System.IO


import qualified RIO.HashMap as M
import qualified RIO.Text as T

--------------------------------------------------------------------------------

type CmdSpec = Text
type FileSpec = Text
type KeyInputSpec = Text
type KeyOutputSpec = Text
type KeyRepeatSpec = Text
type LogLevelSpec = Text
type GestureExpr = Text


-- path ------------------------------------------------------------------------

-- | Root directories we know how to search
data PathRoot
  = XdgCfg      -- ^ The app-configuration directory plus "/kmonad"
  | Home        -- ^ Home directory
  | Custom Text -- ^ Any other path-prefix, may contain globs
  deriving (Eq, Show)

-- | How to look for a particular filepath
data Path = Path
  { _val    :: Text           -- ^ The pattern to match
  , _root   :: Maybe PathRoot -- ^ Optionally a path to be relative to
  , _doGlob :: Bool           -- ^ Whether to glob-match this expression
  } deriving (Eq, Show)
makeLenses ''Path

-- cmd -------------------------------------------------------------------------

data Cmd
  = SimpleCmd Text
  | CompoundCmd FilePath [Text]
  | NoCmd
  deriving (Eq, Show)

 -------------------------------------------------------------------------------

data RunType = FullRun | CfgTest | EvTest
  deriving (Eq, Show)

data CmdAllow = AllCmds | InitCmds | NoCmds
  deriving (Eq, Show)

-- kio -------------------------------------------------------------------------

data KeyInputCfg
  = LinEvdevSrc Path
  | WinHookSrc
  | MacIOKitSrc (Maybe Text)
  | CmdSrc Cmd
  | StdinSrc
  deriving (Eq, Show)

data KeyOutputCfg
  = LinUinputSnk (Maybe Text)
  | WinSendSnk
  | MacKextSink
  | CmdSnk Cmd
  | StdoutSnk
  deriving (Eq, Show)

data KeyRepeatCfg
  = Simulate DelayRate
  | EchoOS
  | IgnoreRepeat
  deriving (Eq, Show)
  -- NOTE^: Here we can add 'detect repeat from OS then ping' idea from github

data LocaleCfg = LocaleCfg
  { _namedCodes :: M.HashMap Name Natural
  , _namedGestures :: M.HashMap Name (Gesture Natural)
  } deriving (Eq, Show)

data RunCfg = RunCfg
  { _cfgPath :: Path
  , _kbdPath :: Path
  , _cmdAllow :: CmdAllow
  , _runType :: RunType
  } deriving (Eq, Show)

data KioCfg = KioCfg
  { _keyRepeatCfg :: KeyRepeatCfg
  , _fallthrough  :: Bool
  , _keyInputCfg  :: KeyInputCfg
  , _keyOutputCfg :: KeyOutputCfg
  , _preKioCmd    :: Cmd
  , _postKioCmd   :: Cmd
  } deriving (Eq, Show)

newtype LogCfg = LogCfg { _logLevel :: LogLevel}
  deriving (Eq, Show)

data AppCfg = AppCfg
  { _appLocaleCfg :: LocaleCfg
  , _appLogCfg :: LogCfg
  , _appKioCfg :: KioCfg
  , _appRunCfg :: RunCfg
  } deriving (Eq, Show)

-- lenses ----------------------------------------------------------------------

makeClassy ''RunCfg
makeClassy ''LocaleCfg
makeClassy ''KioCfg
makeClassy ''LogCfg
makeClassy ''AppCfg

instance HasLogCfg AppCfg where logCfg = appLogCfg
instance HasKioCfg AppCfg where kioCfg = appKioCfg
instance HasRunCfg AppCfg where runCfg = appRunCfg

-- dhall -----------------------------------------------------------------------

-- | The settings that we want to expose to Dhall
--
-- This explicitly leaves out:
-- cfgFile: because it would point at self
-- runType: because it can only be provided by Invoc
--
-- NOTE: the difference between this and 'Invoc', here the only time we use
-- 'Maybe' is to denote the setting of not-doing-something. In 'Invoc' 'Nothing'
-- denotes do-not-change-this-setting. This is because we encode our app
-- defaults *in dhall*. So the default invoc settings are to change nothing, the
-- default CfgFile settings *are* the app defaults.
-- data DhallCfg = DhallCfg
--   { _dcodeNames    :: [DEntry Name Natural]
--   , _dgestureNames :: [DEntry Name GestureExpr]
--   , _dfallthrough  :: Bool
--   , _dcmdAllow     :: Bool
--   , _dlogLevel     :: LogLevelSpec
--   , _dkeyInputCfg  :: KeyInputSpec
--   , _dkeyOutputCfg :: KeyOutputSpec
--   , _dkeymapFile   :: PathExpr
--   , _dkeyRepeat    :: Maybe KeyRepeatSpec
--   , _dpreKioCmd    :: Maybe CmdSpec
--   , _dpostKioCmd   :: Maybe CmdSpec
--   } deriving (Generic, D.FromDhall, Show)
-- makeClassy ''DhallCfg


-- loadDhallCfg :: MonadIO m => FilePath -> m DhallCfg
-- loadDhallCfg f = do
--   let opt = D.defaultInterpretOptions { D.fieldModifier = T.drop 1 }
--   let dec = D.genericAutoWith opt
--   liftIO $ D.inputFile dec f


-- testDhall :: IO ()
-- testDhall = pPrint =<< loadDhallCfg "/home/david/prj/kmonad/cfg/linux.dhall"
