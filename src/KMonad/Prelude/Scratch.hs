-- |

module KMonad.Prelude.Scratch where

import KMonad.Prelude.Imports
import KMonad.Prelude.Definitions (Name, Names)
-- import KMonad.Prelude.Parsing

newtype ParseError = ParseError Int deriving Show
makePrisms ''ParseError

data NameError
  = EmptyName
  | DuplicateNames Names
  deriving Show
makePrisms ''NameError

data LocaleError
  = LocaleParseError ParseError
  | LocaleNameError NameError
  deriving Show
makeClassyPrisms ''LocaleError

data LocaleOops
  = LocaleParseOops ParseError
  | LocaleNameOops NameError
  deriving Show

class AsLocaleOops t where
  _LocaleOops :: Prism' t LocaleOops
  _LocaleParseOops :: Prism' t ParseError
  _LocaleNameOops :: Prism' t NameError

  _LocaleParseOops = _LocaleOops . _LocaleParseOops
  _LocaleNameOops = _LocaleOops . _LocaleNameOops

instance AsLocaleOops LocaleOops



-- instance AsLocaleError ParseError where _LocaleError = _LocaleParseError
-- instance AsLocaleError NameError where _LocaleError =

foo :: LocaleError
foo = _LocaleParseError . _ParseError # (3 :: Int)
