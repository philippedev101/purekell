-- | Purekell: bidirectional Haskell\/PureScript expression translator.
--
-- This is the main entry point for the library. For simple use cases,
-- 'hsToPs' and 'psToHs' translate expression source code between languages:
--
-- >>> hsToPs "fmap f (x, y)"
-- Right "map f (Tuple x y)"
--
-- >>> psToHs "arr.name"
-- Right "name arr"
--
-- For more control, use the 'Codec' type from "Purekell.Codec" together with
-- the language-specific codecs in "Purekell.Haskell" and "Purekell.PureScript".
--
-- For translating typeclass instance method bodies (with pattern arguments and
-- guards), see "Purekell.Instance".
module Purekell
  ( -- * Quick translation
    hsToPs
  , psToHs
    -- * Re-exports
  , module Purekell.AST
  , module Purekell.Codec
  , module Purekell.Instance
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle)

import Purekell.AST
import Purekell.Codec
import Purekell.Haskell (haskellExpr)
import Purekell.Instance
import Purekell.PureScript (purescriptExpr)

-- | Translate a Haskell expression to PureScript.
--
-- Parses the input as a Haskell expression, converts divergent syntax
-- (tuples, record access, cons patterns, etc.), and prints it as PureScript.
--
-- >>> hsToPs "map f xs"
-- Right "map f xs"
--
-- >>> hsToPs "field rec"
-- Right "rec.field"
hsToPs :: Text -> Either (ParseErrorBundle Text Void) Text
hsToPs input = do
  ast <- runParse haskellExpr input
  pure (runPrint purescriptExpr ast)

-- | Translate a PureScript expression to Haskell.
--
-- Parses the input as a PureScript expression, converts divergent syntax
-- (dot-access, @Tuple@ constructors, @Cons@ patterns, etc.), and prints
-- it as Haskell.
--
-- >>> psToHs "arr.name"
-- Right "name arr"
--
-- >>> psToHs "Tuple x y"
-- Right "(x, y)"
psToHs :: Text -> Either (ParseErrorBundle Text Void) Text
psToHs input = do
  ast <- runParse purescriptExpr input
  pure (runPrint haskellExpr ast)
