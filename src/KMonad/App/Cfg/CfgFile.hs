{-# LANGUAGE DeriveAnyClass #-}

module KMonad.App.Cfg.CfgFile where

import KMonad.Prelude
import KMonad.Prelude.Parsing

-- import KMonad.Keyboard
import KMonad.App.Types hiding (AppCfg)
import KMonad.Gesture
import KMonad.App.Cfg.Cfgable
import KMonad.App.Cfg.Expr
import KMonad.App.Cfg.Types

import Data.Either.Validation (Validation(..))
import GHC.Generics (Generic)

import qualified Dhall as D
import qualified RIO.List as L
import qualified RIO.Text as T
import qualified RIO.HashMap as M

-- basic types -----------------------------------------------------------------

-- | NOTE: this should live somewhere else
type Keycode = Natural

-- | An entry in a Dhall Map
data DEntry k v = DEntry
  { mapKey :: k
  , mapValue :: v
  } deriving (Generic, D.FromDhall, Show)
-- NOTE: I would like to use `_` and derive lenses here, but I haven't figured
-- out how to make Dhall modify field-names for anything but the topmost fields.

-- | An iso between 'DEntry' and tuples
-- _Tup :: Iso' (DEntry k v) (k, v)
-- _Tup = iso (\(DEntry k v) -> (k, v)) $ uncurry DEntry

-- | A Dhall Map value
type DMap k v = [ DEntry k v ]

-- | Use '_DMap' as a view of an alist as a DMap, and 'from _DMap' as its inverse
_DMap :: Iso' [(k, v)] (DMap k v)
_DMap = iso (map $ uncurry DEntry) (map $ \(DEntry k v) -> (k, v)) --view mapKey &&& view mapValue)

-- | The entire Dhall configuration
data CfgFile = CfgFile
  { _keycodes :: DMap Name Keycode
  , _gestures :: DMap Name GestureExpr
  , _options  :: DMap Name Text
  , _flags :: [Text]
  } deriving (Generic, D.FromDhall, Show)
makeLenses ''CfgFile

-- locale errors ---------------------------------------------------------------

-- | All the things that can go wrong in locale-specification
data LocaleError
  = CodeNameError NameError
  | GestureNameError NameError
  | OverlappingNames [Name]
  | LocaleParseError ParseError
  | LocaleGestureError [GestureReadError]
  | MissingKeyname [Name]
  deriving (Eq, Show)
makeClassyPrisms ''LocaleError

instance Exception LocaleError
instance AsLocaleError SomeException where _LocaleError = exception

-- IO --------------------------------------------------------------------------

-- | Try to load a 'CfgFile' from disk
loadCfgFile :: MonadIO m => PathExpr -> m CfgFile
loadCfgFile p = do
  pth <- resolve $ p ^. from _PathExpr
  let opt = D.defaultInterpretOptions { D.fieldModifier = T.drop 1 }
  let dec = D.genericAutoWith opt
  liftIO $ D.inputFile dec pth

-- ops -------------------------------------------------------------------------

-- | Try to translate the contents of a traversable, on error return failing key
--
-- This is a very general way of writing code that goes into a Gesture and
-- performs the (Name -> Keycode) mapping.
--
-- A more concrete way of writing this signature would be:
--
-- coded :: (Name -> Maybe Keycode) -> Gesture Name -> Either Name (Gesture Keycode)
--
coded :: Traversable t => (b -> Maybe a) -> t b -> Either b (t a)
coded f x = sequence $ (\n -> maybe (Left n) Right (f n)) <$> x

foo :: CfgFile
foo = CfgFile [ DEntry "a" 1, DEntry "b" 2 ] [ DEntry "A" "a b"] [] []

-- | Contruct a valid LocaleCfg or return an error detailing an issue.
getLocale :: (MonadError e m, AsLocaleError e) => CfgFile -> m LocaleCfg
getLocale  c = do
  let cl = c^.keycodes.from _DMap
  let gl = c^.gestures.from _DMap

  -- Check for duplicates and overlapping names between codes and gestures
  case (checkNames' cl :: Either NameError ()) of
    Left e -> throwError $ _CodeNameError # e
    Right _ -> pure ()
  case (checkNames' gl :: Either NameError ()) of
    Left e -> throwError $ _GestureNameError # e
    Right _ -> pure ()
  _ <- maybe (pure ()) (throwError . (_OverlappingNames #))
         $ duplicates $ (cl^..names) <> (gl^..names)

  -- Create the keycode map
  let codes = M.fromList cl

  -- Check for valid gesture structure
  let (e1, gtxt) = partitionEithers . map readGesture $ gl^..folded._2
  unless (L.null e1) $ throwError (_LocaleGestureError # e1)

  -- Check that gestures only refer to existing keynames
  let (e2, gcode) = partitionEithers . over mapped (coded (`M.lookup` codes)) $ gtxt
  unless (L.null e2) $ throwError (_MissingKeyname # e2)

  -- Put it all together
  pure . LocaleCfg codes . M.fromList . zip (gl^..folded._1) $ gcode


data CfgError = CfgLocaleError LocaleError deriving (Eq, Show)
makeClassyPrisms ''CfgError

instance Exception CfgError
instance AsCfgError SomeException where _CfgError = exception
instance AsLocaleError CfgError where _LocaleError = _CfgLocaleError

-- | Extract the full 'Change AppCfg' from a 'CfgFile'
-- getChange :: AsCfgError e => CfgFile -> Either e (Change AppCfg)
-- getChange c = do
--   l <- getLocale c
--   pure undefined
