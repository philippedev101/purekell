-- | Haskell-specific codecs for expressions, patterns, and literals.
--
-- These codecs parse and print using Haskell syntax conventions:
--
-- * Record access as function application: @field rec@
-- * Tuples with parentheses: @(a, b, c)@
-- * Cons patterns with @:@ operator: @x : xs@
-- * Record fields separated by @=@: @Foo { bar = 1 }@
module Purekell.Haskell
  ( -- * Codecs
    haskellLit
  , haskellExpr
  , haskellPat
  ) where

import Text.Megaparsec (eof)

import Purekell.AST
import Purekell.Codec (Codec (..))
import Purekell.Parser (ExprParsers (..), mkExprParsers, pLit, pPat, sc)
import Purekell.Printer (Target (..), printExpr, printLit, printPat)

hsParsers :: ExprParsers
hsParsers = mkExprParsers pure

-- | Codec for Haskell literals (integers, floats, strings, chars).
haskellLit :: Codec Lit
haskellLit = Codec { codecParser = pLit <* eof, codecPrinter = printLit }

-- | Codec for Haskell expressions.
--
-- Parses and prints the full expression grammar using Haskell syntax.
-- Record access is printed as function application (@field rec@),
-- tuples use parenthesized comma-separated syntax, etc.
haskellExpr :: Codec Expr
haskellExpr = Codec { codecParser = sc *> epExpr hsParsers <* eof, codecPrinter = printExpr Haskell }

-- | Codec for Haskell patterns.
--
-- Cons patterns use the @:@ operator (@x : xs@), tuples use parenthesized
-- comma-separated syntax, etc.
haskellPat :: Codec Pat
haskellPat = Codec { codecParser = sc *> pPat <* eof, codecPrinter = printPat Haskell }
