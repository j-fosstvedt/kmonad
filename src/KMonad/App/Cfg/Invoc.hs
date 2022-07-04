{-# LANGUAGE QuasiQuotes #-}
-- |

module KMonad.App.Cfg.Invoc where

import KMonad.Prelude
import qualified KMonad.Prelude.Parsing as P (_ParseError, ParseError)

import KMonad.App.Cfg.Types hiding (Invoc, HasInvoc(..))
import KMonad.App.Cfg.Cfgable hiding (option, flag)
import KMonad.App.Cfg.Default


-- Imports required to add versioner to command
import KMonad.Args.TH (gitHash)
import Paths_kmonad (version)
import Data.Version (showVersion)

import Options.Applicative

import Text.RawString.QQ
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as L



newtype Invoc = Invoc
  { _cfgChange :: Change AppCfg
  }
makeClassy ''Invoc

-- help-text -------------------------------------------------------------------

-- TODO: Improve help-doc

(<^>) :: Doc -> Doc -> Doc
(<^>) = (L.<$>)

headDoc :: Doc
headDoc = [r|
Start running a KMonad process. See https://github.com/kmonad/kmonad.git for
more extensive documentation. How KMonad acquires and remaps events depends largely
on a dhall configuration file (by default: $XDG_CONFIG_HOME/kmonad/kmonad.dhall)
and a keymap (by default: $XDG_CONFIG_HOME/kmonad/keymap.kbd).
|]

specTxt :: Doc
specTxt = [r|
Many of the configuration options operate on a small DSL further documented on the
github page.
|]

-- IO --------------------------------------------------------------------------

-- | Parse 'Invoc' from the invocation of this program
getInvoc :: MonadIO m => m Invoc
getInvoc = liftIO . customExecParser (prefs showHelpOnEmpty) $
  info (invocP <**> versioner <**> helper)
    (  fullDesc
    <> progDescDoc (Just $ headDoc <^> specTxt)
    <> header   "kmonad - an onion of buttons."
    )

-- | Equip a parser with version information about the program
versioner :: Parser (a -> a)
versioner = infoOption (showVersion version <> ", commit " <> $(gitHash))
  (  long "version"
  <> short 'V'
  <> help "Show version"
  )

-- parsers ---------------------------------------------------------------------

-- | Parse the entire invocation
invocP :: Parser Invoc
invocP = Invoc <$> appMods

-- | Construct flags and options from 'shellFlags' and 'shellOptions'
appMods :: Parser (Change AppCfg)
appMods = let f = filter (hasn't $ source._FromCfgFile) shellFlags
              o = filter (hasn't $ source._FromCfgFile) shellOptions
          in fmap mconcat . sequenceA $ map flagP f <> map optionP o

-- | Turn 1 'AppFlag' into an optparse-applicative 'flag'
flagP :: AppFlag -> Parser (Change AppCfg)
flagP f = flag mempty (f^.change) $ mconcat
  [ long $ unpack (f^.longName)
  , help $ unpack (f^.doc)
  ] <> maybe mempty short (f^.shortName)

-- | Turn 1 'AppOption' into an optparse-applicative 'option'
optionP :: AppOption -> Parser (Change AppCfg)
optionP o = option (eitherReader f) $ mconcat
  [ long $ unpack (o^.longName)
  , help $ unpack (o^.doc)
  , value mempty
  ] <> maybe mempty short (o^.shortName)
  where
    f s = left show $ (o^.mkChange) (pack s)
