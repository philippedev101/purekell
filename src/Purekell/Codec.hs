-- | Bidirectional codecs that pair a megaparsec parser with a pretty-printer.
--
-- A 'Codec' bundles a parser and printer for the same type, making it easy
-- to parse source text into an AST and print it back. This is the core
-- abstraction that enables roundtrip translation between languages.
--
-- See "Purekell.Haskell" and "Purekell.PureScript" for concrete codecs.
module Purekell.Codec
  ( Codec (..)
  , runParse
  , runPrint
  , roundtrip
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, ParseErrorBundle, parse)

-- | A bidirectional codec pairing a parser and printer for type @a@.
--
-- The parser consumes 'Text' input and produces an @a@; the printer
-- converts an @a@ back to 'Text'. When the parser and printer are
-- inverses, @'roundtrip' codec@ is the identity on well-formed values.
data Codec a = Codec
  { codecParser  :: Parsec Void Text a  -- ^ Megaparsec parser for @a@
  , codecPrinter :: a -> Text           -- ^ Pretty-printer for @a@
  }

-- | Parse source text using a codec's parser.
runParse :: Codec a -> Text -> Either (ParseErrorBundle Text Void) a
runParse c = parse (codecParser c) ""

-- | Print a value using a codec's printer.
runPrint :: Codec a -> a -> Text
runPrint = codecPrinter

-- | Parse, print, and re-parse — testing that printed output is stable.
--
-- @roundtrip codec val@ prints @val@ then parses the result. If the codec
-- is well-behaved, @roundtrip codec val == Right val@.
roundtrip :: Codec a -> a -> Either (ParseErrorBundle Text Void) a
roundtrip c = runParse c . runPrint c
