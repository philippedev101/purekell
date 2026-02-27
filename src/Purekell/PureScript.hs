{-# LANGUAGE OverloadedStrings #-}

module Purekell.PureScript
  ( purescriptLit
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

pDotAccess :: Parser Expr -> Parser Expr
pDotAccess atomP = do
  e <- atomP
  fields <- many (char '.' *> pLowerName)
  pure (foldl RecordAccess e fields)

psParsers :: ExprParsers
psParsers = mkExprParsers pDotAccess

purescriptLit :: Codec Lit
purescriptLit = Codec { codecParser = pLit <* eof, codecPrinter = printLit }

purescriptExpr :: Codec Expr
purescriptExpr = Codec { codecParser = sc *> epExpr psParsers <* eof, codecPrinter = printExpr PureScript }

purescriptPat :: Codec Pat
purescriptPat = Codec { codecParser = sc *> pPat <* eof, codecPrinter = printPat PureScript }
