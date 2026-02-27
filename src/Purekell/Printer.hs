{-# LANGUAGE OverloadedStrings #-}

module Purekell.Printer
  ( Target (..)
  , printExpr
  , printLit
  , printPat
  , printPatAtom
  , printAtom
  , printAppFun
  , printInfixArg
  , printInfixLevel
  , printGuard
  , printGuards
  , printCaseAlt
  , printBinding
  , printStmt
  , printType
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Purekell.AST

-- | Target language for printing
data Target = Haskell | PureScript
  deriving (Eq, Show)

-- Qualified name helper

printQual :: [Name] -> Text
printQual ns = T.intercalate "." [m | Name m <- ns]

-- Expression printer

printExpr :: Target -> Expr -> Text
printExpr _ (Literal l) = printLit l
printExpr _ (Var (Name n)) = n
printExpr _ (Con (Name n)) = n
printExpr t (App f x) = printAppFun t f <> " " <> printAtom t x
printExpr t (InfixApp l (Name op) r) =
  printInfixArg t l <> " " <> op <> " " <> printInfixArg t r
printExpr t (Lam pats body) =
  "\\" <> T.intercalate " " (map (printPatAtom t) pats) <> " -> " <> printExpr t body
printExpr t (If c th el) =
  "if " <> printExpr t c <> " then " <> printExpr t th <> " else " <> printExpr t el
printExpr t (Case scrut alts) =
  "case " <> printExpr t scrut <> " of { "
  <> T.intercalate "; " (map (printCaseAlt t) alts)
  <> " }"
printExpr t (Let bindings body) =
  "let { " <> T.intercalate "; " (map (printBinding t) bindings)
  <> " } in " <> printExpr t body
printExpr t (Do stmts) =
  "do { " <> T.intercalate "; " (map (printStmt t) stmts) <> " }"
printExpr t (Neg e) = "-" <> printAppFun t e
printExpr Haskell (RecordAccess rec (Name field)) =
  field <> " " <> printAtom Haskell rec
printExpr PureScript (RecordAccess rec (Name field)) =
  printAtom PureScript rec <> "." <> field
printExpr Haskell (Tuple es) =
  "(" <> T.intercalate ", " (map (printExpr Haskell) es) <> ")"
printExpr PureScript (Tuple [a, b]) =
  "Tuple " <> printAtom PureScript a <> " " <> printAtom PureScript b
printExpr PureScript (Tuple (a : rest)) =
  "Tuple " <> printAtom PureScript a <> " " <> printAtom PureScript (Tuple rest)
printExpr PureScript (Tuple []) =
  error "Tuple must have at least 2 elements"
printExpr _ (ListLit []) = "[]"
printExpr t (ListLit es) = "[" <> T.intercalate ", " (map (printExpr t) es) <> "]"
printExpr t (LeftSection e (Name op)) = "(" <> printInfixArg t e <> " " <> op <> ")"
printExpr t (RightSection (Name op) e) = "(" <> op <> " " <> printInfixArg t e <> ")"
printExpr t (Where e bs) =
  printInfixLevel t e <> " where { " <> T.intercalate "; " (map (printBinding t) bs) <> " }"
printExpr t (Ann e ty) =
  printInfixLevel t e <> " :: " <> printType ty
printExpr t (RecordUpdate e fields) =
  printAtom t e <> " { " <> T.intercalate ", " (map printField fields) <> " }"
  where printField (Name n, val) = n <> " = " <> printExpr t val
printExpr _ (QVar qual (Name n)) = printQual qual <> "." <> n
printExpr _ (QCon qual (Name n)) = printQual qual <> "." <> n

-- Parenthesization helpers

isCompound :: Target -> Expr -> Bool
isCompound Haskell    (Tuple {}) = False
isCompound PureScript (Tuple {}) = True
isCompound _ (ListLit {}) = False
isCompound _ (LeftSection {}) = False
isCompound _ (RightSection {}) = False
isCompound _ (Neg {})  = True
isCompound Haskell    (RecordAccess {}) = True
isCompound PureScript (RecordAccess {}) = False
isCompound _ (App {})      = True
isCompound _ (InfixApp {}) = True
isCompound _ (Lam {})      = True
isCompound _ (If {})       = True
isCompound _ (Case {})     = True
isCompound _ (Let {})      = True
isCompound _ (Do {})       = True
isCompound _ (Where {})    = True
isCompound _ (Ann {})      = True
isCompound _ _             = False

printAtom :: Target -> Expr -> Text
printAtom t e
  | isCompound t e = "(" <> printExpr t e <> ")"
  | otherwise      = printExpr t e

printAppFun :: Target -> Expr -> Text
printAppFun t e@(App {}) = printExpr t e  -- left-assoc app doesn't need parens on left
printAppFun t e
  | isCompound t e = "(" <> printExpr t e <> ")"
  | otherwise      = printExpr t e

printInfixArg :: Target -> Expr -> Text
printInfixArg t e@(InfixApp {}) = "(" <> printExpr t e <> ")"
printInfixArg t e@(Lam {})      = "(" <> printExpr t e <> ")"
printInfixArg t e@(If {})       = "(" <> printExpr t e <> ")"
printInfixArg t e@(Case {})     = "(" <> printExpr t e <> ")"
printInfixArg t e@(Let {})      = "(" <> printExpr t e <> ")"
printInfixArg t e@(Do {})       = "(" <> printExpr t e <> ")"
printInfixArg t e@(Where {})    = "(" <> printExpr t e <> ")"
printInfixArg t e@(Ann {})      = "(" <> printExpr t e <> ")"
printInfixArg t e               = printExpr t e

printInfixLevel :: Target -> Expr -> Text
printInfixLevel t e@(Lam {})  = "(" <> printExpr t e <> ")"
printInfixLevel t e@(If {})   = "(" <> printExpr t e <> ")"
printInfixLevel t e@(Case {}) = "(" <> printExpr t e <> ")"
printInfixLevel t e@(Let {})  = "(" <> printExpr t e <> ")"
printInfixLevel t e@(Do {})   = "(" <> printExpr t e <> ")"
printInfixLevel t e@(Where {}) = "(" <> printExpr t e <> ")"
printInfixLevel t e@(Ann {})   = "(" <> printExpr t e <> ")"
printInfixLevel t e           = printExpr t e

-- Guard / case alt / binding / stmt printers

printGuard :: Target -> Guard -> Text
printGuard t (Guard e) = "| " <> printInfixLevel t e

printGuards :: Target -> [Guard] -> Text
printGuards _ [] = ""
printGuards t gs = " " <> T.intercalate " " (map (printGuard t) gs)

printCaseAlt :: Target -> CaseAlt -> Text
printCaseAlt t (CaseAlt pat guards body) =
  printPat t pat <> printGuards t guards <> " -> " <> printExpr t body

printBinding :: Target -> Binding -> Text
printBinding t (Binding pat body) = printPat t pat <> " = " <> printExpr t body

printStmt :: Target -> Stmt -> Text
printStmt t (StmtBind pat body) = printPat t pat <> " <- " <> printExpr t body
printStmt t (StmtExpr e) = printExpr t e
printStmt t (StmtLet bindings) =
  "let { " <> T.intercalate "; " (map (printBinding t) bindings) <> " }"

-- Type printer

printType :: Type -> Text
printType (TyCon (Name n)) = n
printType (TyVar (Name n)) = n
printType (TyApp f x) = printTyAppFun f <> " " <> printTyAtom x
printType (TyFun a b) = printTyFunArg a <> " -> " <> printType b
printType (TyQCon qual (Name n)) = printQual qual <> "." <> n

printTyAtom :: Type -> Text
printTyAtom ty@(TyApp {}) = "(" <> printType ty <> ")"
printTyAtom ty@(TyFun {}) = "(" <> printType ty <> ")"
printTyAtom ty = printType ty

printTyAppFun :: Type -> Text
printTyAppFun ty@(TyFun {}) = "(" <> printType ty <> ")"
printTyAppFun ty = printType ty

printTyFunArg :: Type -> Text
printTyFunArg ty@(TyFun {}) = "(" <> printType ty <> ")"
printTyFunArg ty = printType ty

-- Literal printer

printLit :: Lit -> Text
printLit (IntLit n) = T.pack (show n)
printLit (FloatLit d) = T.pack (show d)
printLit (StringLit s) = "\"" <> escapeString s <> "\""
printLit (CharLit c) = "'" <> escapeChar c <> "'"

escapeString :: Text -> Text
escapeString = T.concatMap escapeStringChar
  where
    escapeStringChar '"'  = "\\\""
    escapeStringChar '\\' = "\\\\"
    escapeStringChar '\n' = "\\n"
    escapeStringChar '\t' = "\\t"
    escapeStringChar c    = T.singleton c

escapeChar :: Char -> Text
escapeChar '\'' = "\\'"
escapeChar '\\' = "\\\\"
escapeChar '\n' = "\\n"
escapeChar '\t' = "\\t"
escapeChar c    = T.singleton c

-- Pattern printers

printPat :: Target -> Pat -> Text
printPat _ (VarPat (Name n)) = n
printPat _ (LitPat l) = printLit l
printPat _ WildPat = "_"
printPat _ (ConPat (Name n) []) = n
printPat t (ConPat (Name n) args) = n <> " " <> T.intercalate " " (map (printPatAtom t) args)
printPat Haskell (TuplePat ps) =
  "(" <> T.intercalate ", " (map (printPat Haskell) ps) <> ")"
printPat PureScript (TuplePat [a, b]) =
  "Tuple " <> printPatAtom PureScript a <> " " <> printPatAtom PureScript b
printPat PureScript (TuplePat (a : rest)) =
  "Tuple " <> printPatAtom PureScript a <> " " <> printPatAtom PureScript (TuplePat rest)
printPat PureScript (TuplePat []) =
  error "TuplePat must have at least 2 elements"
printPat _ (ListPat []) = "[]"
printPat t (ListPat ps) = "[" <> T.intercalate ", " (map (printPat t) ps) <> "]"
printPat Haskell (ConsPat l r) = printPatAtom Haskell l <> " : " <> printPat Haskell r
printPat PureScript (ConsPat l r) = "Cons " <> printPatAtom PureScript l <> " " <> printPatAtom PureScript r
printPat t (AsPat (Name n) p) = n <> "@" <> printPatAtom t p
printPat _ (NegLitPat l) = "-" <> printLit l

printPatAtom :: Target -> Pat -> Text
printPatAtom t p@(ConPat _ (_:_)) = "(" <> printPat t p <> ")"
printPatAtom t p@(TuplePat _) = case t of
  Haskell    -> printPat t p  -- already parenthesized
  PureScript -> "(" <> printPat t p <> ")"
printPatAtom t p@(ConsPat _ _) = "(" <> printPat t p <> ")"
printPatAtom t p@(AsPat _ _) = "(" <> printPat t p <> ")"
printPatAtom t p@(NegLitPat _) = "(" <> printPat t p <> ")"
printPatAtom t p = printPat t p
