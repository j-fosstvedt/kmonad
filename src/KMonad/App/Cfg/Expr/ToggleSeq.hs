-- |

module KMonad.App.Cfg.Expr.ToggleSeq where

import K.Initial
import K.Initial.Parsing
import K.Gesture

import Data.Char
import qualified RIO.Seq  as Q


-- parser ----------------------------------------------------------------------

-- | Parse a sequence of elements into a 'ToggleSeq' of 'Text'
tseq :: Parser (ToggleSeq Name)
tseq = fmap mconcat . some . lex . choice $
  [subg, try openTag, try around_, try closeTag, tap_]

-- | Characters that may not occur in tag-names
reserved :: [Char]
reserved = "()-~[]"

-- | Parse a series of valid characters as a tag
tag_ :: Parser Text
tag_ = takeWhile1P (Just "tag-character") f
  where f c = not $ isSpace c || c `elem` reserved

-- | Parse a "S-" sequence as 1 tag around another
around_ :: Parser Gest
around_ = do
  a <- tag_
  _ <- char '-'
  b <- try around_ <|> subg <|> tap_
  pure $ (On a <| b) |> Off a

-- | Parse a ")-X" as an OFF-toggle
closeTag :: Parser Gest
closeTag = do
  _ <- string ")-"
  a <- tag_
  pure . Q.singleton $ Off a

-- | Parse a "X-(" as an ON-toggle
openTag :: Parser Gest
openTag = do
  a <- tag_
  _ <- string "-("
  pure . Q.singleton $ On a

-- | Parse only a tag as a tap of that element
tap_ :: Parser Gest
tap_ = do
  a <- tag_
  pure . Q.fromList $ [On a, Off 
]

-- | Parse a [] delimited series as a nested gesture
subg :: Parser Gest
subg = do
  _ <- char '['
  g <- tseq
  _ <- char ']'
  pure g

