-- | PureScript-specific codecs for expressions, patterns, and literals.
--
-- These codecs parse and print using PureScript syntax conventions:
--
-- * Record access with dot notation: @rec.field@
-- * Tuples as nested @Tuple@ constructors: @Tuple a (Tuple b c)@
-- * Cons patterns as @Cons@ constructor: @Cons x xs@
-- * Record fields separated by @:@ in constructor contexts: @Foo { bar: 1 }@
module Purekell.PureScript
  ( -- * Codecs
    purescriptLit
  , purescriptExpr
  , purescriptPat
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, many)
import Text.Megaparsec.Char (char)

import Purekell.AST
import Purekell.Codec (Codec (..))
import Purekell.Parser (ExprParsers (..), mkExprParsers, pLit, pLowerName, pPat, sc)
import Purekell.Printer (Target (..), printExpr, printLit, printPat)

type Parser = Parsec Void Text

-- | Parse a chain of dot-accessed fields: @.foo.bar.baz@
pDotAccess :: Expr -> Parser Expr
pDotAccess e = do
  fields <- many (char '.' *> pLowerName)
  pure (foldl RecordAccess e fields)

psParsers :: ExprParsers
psParsers = mkExprParsers pDotAccess

-- | Codec for PureScript literals (integers, floats, strings, chars).
purescriptLit :: Codec Lit
purescriptLit = Codec { codecParser = pLit <* eof, codecPrinter = printLit }

-- | Codec for PureScript expressions.
--
-- Parses and prints the full expression grammar using PureScript syntax.
-- Record access uses dot notation (@rec.field@), tuples use the @Tuple@
-- constructor, etc.
purescriptExpr :: Codec Expr
purescriptExpr = Codec { codecParser = sc *> epExpr psParsers <* eof, codecPrinter = printExpr PureScript }

-- | Codec for PureScript patterns.
--
-- Cons patterns use the @Cons@ constructor (@Cons x xs@), tuples use
-- the @Tuple@ constructor, etc.
purescriptPat :: Codec Pat
purescriptPat = Codec { codecParser = sc *> pPat <* eof, codecPrinter = printPat PureScript }
