{-# LANGUAGE OverloadedStrings #-}

module Purekell.Parser
  ( ExprParsers (..)
  , mkExprParsers
  , sc
  , lexeme
  , symbol
  , keyword
  , pLit
  , pLowerName
  , pUpperName
  , pOperator
  , pAtomPat
  , pConPat
  , pPat
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Purekell.AST

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

keyword :: Text -> Parser ()
keyword w = lexeme (string w *> notFollowedBy (alphaNumChar <|> char '_' <|> char '\''))

-- Literal parsers

pIntLit :: Parser Lit
pIntLit = IntLit <$> lexeme L.decimal

pCharLit :: Parser Lit
pCharLit = CharLit <$> lexeme (between (char '\'') (char '\'') L.charLiteral)

pStringLit :: Parser Lit
pStringLit = StringLit . T.pack <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

pLit :: Parser Lit
pLit = pCharLit <|> pStringLit <|> pIntLit

-- Name parsers

pLowerName :: Parser Name
pLowerName = lexeme $ try $ do
  c <- lowerChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_' <|> char '\'')
  let n = T.pack (c : cs)
  if n `elem` reserved then fail ("reserved: " ++ T.unpack n) else pure (Name n)
  where
    reserved = ["case", "of", "let", "in", "where", "do", "if", "then", "else", "_"]

pUpperName :: Parser Name
pUpperName = lexeme $ do
  c <- upperChar
  cs <- many (alphaNumChar <|> char '_' <|> char '\'')
  pure (Name (T.pack (c : cs)))

pOperator :: Parser Name
pOperator = lexeme $ try $ do
  op <- some (oneOf ("!#$%&*+./<=>?@\\^|-~:" :: [Char]))
  let n = T.pack op
  if n `elem` ["->", "|", "<-", "="] then fail "reserved operator" else pure (Name n)

-- Pattern parsers

pAtomPat :: Parser Pat
pAtomPat = choice
  [ LitPat <$> pLit
  , WildPat <$ lexeme (char '_' <* notFollowedBy (alphaNumChar <|> char '_' <|> char '\''))
  , ConPat <$> pUpperName <*> pure []
  , VarPat <$> pLowerName
  , between (symbol "(") (symbol ")") pPat
  ]

pConPat :: Parser Pat
pConPat = ConPat <$> pUpperName <*> many pAtomPat

pPat :: Parser Pat
pPat = pConPat <|> pAtomPat

-- Expression parsers

data ExprParsers = ExprParsers
  { epExpr  :: Parser Expr
  , epGuard :: Parser Guard
  }

-- | Build expression parsers. The wrapper transforms the atom parser
-- (identity for Haskell, dot-access chaining for PureScript).
mkExprParsers :: (Parser Expr -> Parser Expr) -> ExprParsers
mkExprParsers wrapAtom = ExprParsers { epExpr = expr, epGuard = guard }
  where
    atom = choice
      [ Literal <$> pLit
      , Con <$> pUpperName
      , Var <$> pLowerName
      , between (symbol "(") (symbol ")") expr
      ]
    wrappedAtom = wrapAtom atom
    appExpr = do
      f <- wrappedAtom
      args <- many wrappedAtom
      pure (foldl App f args)
    pPrefixMinus = lexeme (try (char '-' <* notFollowedBy (oneOf ("!#$%&*+./<=>?@\\^|-~:" :: [Char]))))
    prefixExpr = (Neg <$> (pPrefixMinus *> appExpr)) <|> appExpr
    infixExpr = do
      first <- prefixExpr
      rest <- many ((,) <$> pOperator <*> prefixExpr)
      pure (foldl (\l (op, r) -> InfixApp l op r) first rest)
    guard = symbol "|" *> (Guard <$> infixExpr)
    lam = Lam <$> (symbol "\\" *> some pAtomPat) <*> (symbol "->" *> expr)
    ifE = If <$> (keyword "if" *> expr) <*> (keyword "then" *> expr) <*> (keyword "else" *> expr)
    caseAlt = CaseAlt <$> pPat <*> many guard <*> (symbol "->" *> expr)
    caseE = Case <$> (keyword "case" *> expr <* keyword "of" <* symbol "{")
                 <*> (caseAlt `sepBy1` symbol ";" <* symbol "}")
    binding = Binding <$> pPat <*> (symbol "=" *> expr)
    letE = Let <$> (keyword "let" *> symbol "{" *> binding `sepBy1` symbol ";" <* symbol "}")
               <*> (keyword "in" *> expr)
    stmtBind = StmtBind <$> pPat <*> (symbol "<-" *> expr)
    stmtLet = keyword "let" *> symbol "{" *> (StmtLet <$> binding `sepBy1` symbol ";")
              <* symbol "}" <* notFollowedBy (keyword "in")
    stmtExpr = StmtExpr <$> expr
    stmt = try stmtBind <|> try stmtLet <|> stmtExpr
    doE = Do <$> (keyword "do" *> symbol "{" *> stmt `sepBy1` symbol ";" <* symbol "}")
    expr = lam <|> try ifE <|> try caseE <|> try letE <|> try doE <|> infixExpr
