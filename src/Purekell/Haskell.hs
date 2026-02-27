{-# LANGUAGE OverloadedStrings #-}

module Purekell.Haskell
  ( haskellLit
  , haskellExpr
  , haskellPat
  ) where

import Text.Megaparsec (eof)

import Purekell.AST
import Purekell.Codec (Codec (..))
import Purekell.Parser (ExprParsers (..), mkExprParsers, pLit, pPat, sc)
import Purekell.Printer (Target (..), printExpr, printLit, printPat)

hsParsers :: ExprParsers
hsParsers = mkExprParsers id

haskellLit :: Codec Lit
haskellLit = Codec { codecParser = pLit <* eof, codecPrinter = printLit }

haskellExpr :: Codec Expr
haskellExpr = Codec { codecParser = sc *> epExpr hsParsers <* eof, codecPrinter = printExpr Haskell }

haskellPat :: Codec Pat
haskellPat = Codec { codecParser = sc *> pPat <* eof, codecPrinter = printPat }
