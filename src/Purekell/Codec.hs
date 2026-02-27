module Purekell.Codec
  ( Codec (..)
  , runParse
  , runPrint
  , roundtrip
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, ParseErrorBundle, parse)

data Codec a = Codec
  { codecParser  :: Parsec Void Text a
  , codecPrinter :: a -> Text
  }

runParse :: Codec a -> Text -> Either (ParseErrorBundle Text Void) a
runParse c = parse (codecParser c) ""

runPrint :: Codec a -> a -> Text
runPrint = codecPrinter

roundtrip :: Codec a -> a -> Either (ParseErrorBundle Text Void) a
roundtrip c = runParse c . runPrint c
