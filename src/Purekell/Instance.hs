{-# LANGUAGE OverloadedStrings #-}

module Purekell.Instance
  ( MethodEquation (..)
  , Target (..)
  , parseMethodBody
  , printMethodBody
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, ParseErrorBundle, eof, many, parse, sepBy1)

import Purekell.AST
import Purekell.Parser (ExprParsers (..), mkExprParsers, pAtomPat, pLowerName, sc, symbol)
import Purekell.Printer (Target (..), printExpr, printGuards, printPatAtom)

type Parser = Parsec Void Text

-- | A single method equation: methodName pat1 pat2 = expr
data MethodEquation = MethodEquation
  { methodName :: Name
  , methodPats :: [Pat]
  , methodGuards :: [Guard]
  , methodBody :: Expr
  } deriving (Eq, Show)

instanceParsers :: ExprParsers
instanceParsers = mkExprParsers pure

pMethodEquation :: Parser MethodEquation
pMethodEquation = do
  name <- pLowerName
  pats <- many pAtomPat
  guards <- many (epGuard instanceParsers)
  _ <- symbol "="
  body <- epExpr instanceParsers
  pure (MethodEquation name pats guards body)

-- | Parse a method body (one or more equations separated by semicolons or newlines)
parseMethodBody :: Text -> Either (ParseErrorBundle Text Void) [MethodEquation]
parseMethodBody = parse (sc *> pMethodEquation `sepBy1` symbol ";" <* eof) ""

-- | Print method equations for a target language
printMethodBody :: Target -> [MethodEquation] -> Text
printMethodBody target eqs = T.intercalate "\n" (map (printMethodEq target) eqs)

printMethodEq :: Target -> MethodEquation -> Text
printMethodEq target (MethodEquation (Name name) pats guards body) =
  name <> patsText <> guardsText <> " = " <> printExpr target body
  where
    patsText
      | null pats = ""
      | otherwise = " " <> T.intercalate " " (map (printPatAtom target) pats)
    guardsText = printGuards target guards
