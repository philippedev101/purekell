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
  , pFieldSep
  , pAtomPat
  , pConPat
  , pPat
  , pType
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

pFloatLit :: Parser Lit
pFloatLit = FloatLit <$> lexeme (try L.float)

pCharLit :: Parser Lit
pCharLit = CharLit <$> lexeme (between (char '\'') (char '\'') L.charLiteral)

pStringLit :: Parser Lit
pStringLit = StringLit . T.pack <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

pLit :: Parser Lit
pLit = pCharLit <|> pStringLit <|> pFloatLit <|> pIntLit

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

-- Raw name parsers (no trailing whitespace, for qualified name components)

pRawUpperIdent :: Parser Text
pRawUpperIdent = do
  c <- upperChar
  cs <- many (alphaNumChar <|> char '_' <|> char '\'')
  pure (T.pack (c : cs))

pRawLowerIdent :: Parser Text
pRawLowerIdent = try $ do
  c <- lowerChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_' <|> char '\'')
  let n = T.pack (c : cs)
  if n `elem` reserved then fail ("reserved: " ++ T.unpack n) else pure n
  where
    reserved = ["case", "of", "let", "in", "where", "do", "if", "then", "else", "_"]

-- Qualified name parsers

pQualifiedOrCon :: Parser Expr
pQualifiedOrCon = lexeme $ do
  first <- pRawUpperIdent
  rest <- many (try (char '.' *> pRawUpperIdent))
  mLower <- optional (try (char '.' *> pRawLowerIdent))
  let allUpper = first : rest
  case mLower of
    Just low -> pure (QVar (map Name allUpper) (Name low))
    Nothing  -> case allUpper of
      [one] -> pure (Con (Name one))
      _     -> pure (QCon (map Name (init allUpper)) (Name (last allUpper)))

pQualifiedOrTyCon :: Parser Type
pQualifiedOrTyCon = lexeme $ do
  first <- pRawUpperIdent
  rest <- many (try (char '.' *> pRawUpperIdent))
  let allUpper = first : rest
  case allUpper of
    [one] -> pure (TyCon (Name one))
    _     -> pure (TyQCon (map Name (init allUpper)) (Name (last allUpper)))

pOperator :: Parser Name
pOperator = lexeme $ try $ do
  op <- some (oneOf ("!#$%&*+./<=>?@\\^|-~:" :: [Char]))
  let n = T.pack op
  if n `elem` ["->", "|", "<-", "=", "::"] then fail "reserved operator" else pure (Name n)

-- Pattern parsers

pPatMinus :: Parser ()
pPatMinus = () <$ lexeme (try (char '-' <* notFollowedBy (oneOf ("!#$%&*+./<=>?@\\^|-~:" :: [Char]))))

pNumLit :: Parser Lit
pNumLit = pFloatLit <|> pIntLit

pAtomPat :: Parser Pat
pAtomPat = choice
  [ NegLitPat <$> (pPatMinus *> pNumLit)
  , LitPat <$> pLit
  , WildPat <$ lexeme (char '_' <* notFollowedBy (alphaNumChar <|> char '_' <|> char '\''))
  , ConPat <$> pUpperName <*> pure []
  , try (AsPat <$> pLowerName <*> (symbol "@" *> pAtomPat))
  , VarPat <$> pLowerName
  , pParenOrTuplePat
  , pListPat
  ]

pListPat :: Parser Pat
pListPat = ListPat <$> (symbol "[" *> pPat `sepBy` symbol "," <* symbol "]")

pParenOrTuplePat :: Parser Pat
pParenOrTuplePat = do
  _ <- symbol "("
  p <- pPat
  rest <- many (symbol "," *> pPat)
  _ <- symbol ")"
  pure $ case rest of
    [] -> p
    _  -> TuplePat (p : rest)

pRecordPatFields :: Parser [(Name, Pat)]
pRecordPatFields = symbol "{" *> fieldPatAssign `sepBy1` symbol "," <* symbol "}"

fieldPatAssign :: Parser (Name, Pat)
fieldPatAssign = (,) <$> pLowerName <*> (pFieldSep *> pPat)

pConPat :: Parser Pat
pConPat = do
  name <- pUpperName
  (RecordPat name <$> try pRecordPatFields) <|> (ConPat name <$> many pAtomPat)

pConsOp :: Parser ()
pConsOp = lexeme $ try $ () <$ char ':' <* notFollowedBy (oneOf ("!#$%&*+./<=>?@\\^|-~:" :: [Char]))

pFieldSep :: Parser ()
pFieldSep = () <$ symbol "="
  <|> () <$ lexeme (try (char ':' <* notFollowedBy (oneOf ("!#$%&*+./<=>?@\\^|-~:" :: [Char]))))

pPat :: Parser Pat
pPat = do
  left <- pConPat <|> pAtomPat
  rest <- optional (pConsOp *> pPat)
  pure $ case rest of
    Nothing -> left
    Just r  -> ConsPat left r

-- Type parsers

pType :: Parser Type
pType = pTyFun

pTyFun :: Parser Type
pTyFun = do
  t <- pTyApp
  rest <- optional (symbol "->" *> pTyFun)  -- right-associative
  pure $ case rest of
    Nothing -> t
    Just r  -> TyFun t r

pTyApp :: Parser Type
pTyApp = do
  f <- pTyAtom
  args <- many pTyAtom
  pure (foldl TyApp f args)

pTyAtom :: Parser Type
pTyAtom = choice
  [ pQualifiedOrTyCon
  , TyVar <$> pLowerName
  , symbol "(" *> pType <* symbol ")"
  ]

pDoubleColon :: Parser ()
pDoubleColon = () <$ lexeme (try (string "::" <* notFollowedBy (oneOf ("!#$%&*+./<=>?@\\^|-~:" :: [Char]))))

-- Expression parsers

data ExprParsers = ExprParsers
  { epExpr  :: Parser Expr
  , epGuard :: Parser Guard
  }

-- | Build expression parsers. The postfix callback adds extra postfix
-- operations after each atom (identity for Haskell, dot-access chaining
-- for PureScript). It interleaves with record update parsing.
mkExprParsers :: (Expr -> Parser Expr) -> ExprParsers
mkExprParsers postfix = ExprParsers { epExpr = expr, epGuard = guard }
  where
    atom = choice
      [ Literal <$> pLit
      , pQualifiedOrCon
      , Var <$> pLowerName
      , pParenOrTupleOrSection
      , pList
      ]
    pList = ListLit <$> (symbol "[" *> expr `sepBy` symbol "," <* symbol "]")
    pNonMinusOp = lexeme $ try $ do
      op <- some (oneOf ("!#$%&*+./<=>?@\\^|-~:" :: [Char]))
      let n = T.pack op
      if n `elem` ["->", "|", "<-", "=", "-", "::"] then fail "reserved/excluded operator" else pure (Name n)
    pParenOrTupleOrSection = do
      _ <- symbol "("
      choice
        [ -- Right section: (op expr) — exclude solo minus to avoid (-x) conflict
          try (RightSection <$> pNonMinusOp <*> expr <* symbol ")")
        , -- Parse prefix-level expr, then decide (for left sections + infix)
          do e <- prefixExpr
             choice
               [ -- Left section: expr op )
                 try (LeftSection e <$> pOperator <* symbol ")")
               , -- Infix continuation → then optional ann, then optional where, then tuple or grouping
                 do rest <- many ((,) <$> pOperator <*> prefixExpr)
                    let infE = foldl (\l (op, r) -> InfixApp l op r) e rest
                    annE <- pOptionalAnn infE
                    fullE <- pOptionalWhere annE
                    choice
                      [ Tuple . (fullE :) <$> some (symbol "," *> expr) <* symbol ")"
                      , fullE <$ symbol ")"
                      ]
               ]
        , -- Non-prefix expressions (lambda, if, case, let, do) inside parens
          do e <- lam <|> try ifE <|> try caseE <|> try letE <|> try doE
             e1 <- pOptionalAnn e
             e' <- pOptionalWhere e1
             choice
               [ Tuple . (e' :) <$> some (symbol "," *> expr) <* symbol ")"
               , e' <$ symbol ")"
               ]
        ]
    fieldAssign = (,) <$> pLowerName <*> (pFieldSep *> expr)
    postfixChain e = do
      e1 <- postfix e
      updates <- many (try (symbol "{" *> fieldAssign `sepBy1` symbol "," <* symbol "}"))
      case updates of
        [] -> pure e1
        _  -> postfixChain (foldl RecordUpdate e1 updates)
    atomWithPostfix = atom >>= postfixChain
    appExpr = do
      f <- atomWithPostfix
      args <- many atomWithPostfix
      pure (foldl App f args)
    pPrefixMinus = lexeme (try (char '-' <* notFollowedBy (oneOf ("!#$%&*+./<=>?@\\^|-~:" :: [Char]))))
    prefixExpr = (Neg <$> (pPrefixMinus *> appExpr)) <|> appExpr
    infixExpr = do
      first <- prefixExpr
      rest <- many ((,) <$> pOperator <*> prefixExpr)
      pure (foldl (\l (op, r) -> InfixApp l op r) first rest)
    pOptionalAnn e = do
      mty <- optional (pDoubleColon *> pType)
      pure $ case mty of
        Nothing -> e
        Just ty -> Ann e ty
    annExpr = infixExpr >>= pOptionalAnn
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
    pOptionalWhere e = do
      mbs <- optional (keyword "where" *> symbol "{" *> binding `sepBy1` symbol ";" <* symbol "}")
      pure $ case mbs of
        Nothing -> e
        Just bs -> Where e bs
    whereExpr = annExpr >>= pOptionalWhere
    expr = lam <|> try ifE <|> try caseE <|> try letE <|> try doE <|> whereExpr
