-- | Parsing and printing of typeclass instance method equations.
--
-- This module handles the specific format of method bodies as they appear
-- in typeclass instances:
--
-- @
-- methodName pat1 pat2 | guard1 | guard2 = body
-- @
--
-- Multiple equations can be separated by semicolons:
--
-- @
-- fromEnum Sunday = 0; fromEnum Monday = 1
-- @
--
-- This is the primary use case for purekell: translating instance method
-- bodies between Haskell and PureScript when generating bridge code.
module Purekell.Instance
  ( -- * Method equations
    MethodEquation (..)
    -- * Target language
  , Target (..)
    -- * Parsing and printing
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

-- | A single method equation: @methodName pat1 pat2 | guard = body@.
--
-- Represents one clause of a method definition, with the method name,
-- pattern arguments, optional guards, and the right-hand side expression.
data MethodEquation = MethodEquation
  { methodName   :: Name      -- ^ The method name (e.g., @show@, @fromEnum@)
  , methodPats   :: [Pat]     -- ^ Pattern arguments (may be empty)
  , methodGuards :: [Guard]   -- ^ Guard conditions (may be empty)
  , methodBody   :: Expr      -- ^ The right-hand side expression
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

-- | Parse a method body consisting of one or more equations separated
-- by semicolons.
--
-- >>> parseMethodBody "show True = \"True\"; show False = \"False\""
-- Right [MethodEquation ...]
parseMethodBody :: Text -> Either (ParseErrorBundle Text Void) [MethodEquation]
parseMethodBody = parse (sc *> pMethodEquation `sepBy1` symbol ";" <* eof) ""

-- | Print method equations for a target language.
--
-- Multiple equations are separated by newlines.
--
-- >>> printMethodBody Haskell eqs
-- "show True = \"True\"\nshow False = \"False\""
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
