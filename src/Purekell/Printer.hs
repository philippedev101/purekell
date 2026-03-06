-- | Pretty-printers for the shared AST.
--
-- Converts AST nodes back to source text for a given 'Target' language.
-- The printer handles all divergent syntax automatically:
--
-- * __Record access__: @field rec@ (Haskell) vs @rec.field@ (PureScript)
-- * __Tuples__: @(a, b)@ (Haskell) vs @Tuple a b@ (PureScript)
-- * __Cons patterns__: @x : xs@ (Haskell) vs @Cons x xs@ (PureScript)
-- * __Record field separators__: @=@ (Haskell) vs @:@ (PureScript, constructor context)
--
-- The printer inserts parentheses as needed to preserve meaning.
module Purekell.Printer
  ( -- * Target language
    Target (..)
    -- * Operator printing
  , printOp
    -- * Expression printing
  , printExpr
  , printLit
    -- * Pattern printing
  , printPat
  , printPatAtom
    -- * Parenthesization helpers
  , printAtom
  , printAppFun
  , printInfixArg
  , printInfixLevel
    -- * Guard, case alt, binding, and statement printing
  , printGuard
  , printGuards
  , printCaseAlt
  , printBinding
  , printStmt
    -- * Type printing
  , printType
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Purekell.AST

-- | Target language for printing. Determines syntax choices for
-- divergent constructs like record access, tuples, and cons patterns.
data Target = Haskell | PureScript
  deriving (Eq, Show)

-- Qualified name helper

printQual :: [Name] -> Text
printQual ns = T.intercalate "." [m | Name m <- ns]

-- | Print an operator name. Symbolic operators are printed bare (@+@, @>>=@);
-- alphanumeric operators are wrapped in backticks (@\`div\`@, @\`elem\`@).
printOp :: Name -> Text
printOp (Name n)
  | T.all isSymChar n = n
  | otherwise         = "`" <> n <> "`"
  where
    isSymChar c = c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: [Char])

-- | Print an expression for the given target language.
--
-- This is the main entry point for expression printing. It dispatches
-- to the appropriate syntax based on the 'Target' and the AST node.
printExpr :: Target -> Expr -> Text
printExpr _ (Literal l) = printLit l
printExpr _ (Var (Name n)) = n
printExpr _ (Con (Name n)) = n
printExpr t (App f x) = printAppFun t f <> " " <> printAtom t x
printExpr t (InfixApp l op r) =
  printInfixArg t l <> " " <> printOp op <> " " <> printInfixArg t r
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
printExpr t (LeftSection e op) = "(" <> printInfixArg t e <> " " <> printOp op <> ")"
printExpr t (RightSection op e) = "(" <> printOp op <> " " <> printInfixArg t e <> ")"
printExpr t (Where e bs) =
  printInfixLevel t e <> " where { " <> T.intercalate "; " (map (printBinding t) bs) <> " }"
printExpr t (Ann e ty) =
  printInfixLevel t e <> " :: " <> printType ty
printExpr t (RecordUpdate e fields) =
  printAtom t e <> " { " <> T.intercalate ", " (map (printRecField t e) fields) <> " }"
printExpr _ (QVar qual (Name n)) = printQual qual <> "." <> n
printExpr _ (QCon qual (Name n)) = printQual qual <> "." <> n

printRecField :: Target -> Expr -> (Name, Expr) -> Text
printRecField t base (Name n, val)
  | PureScript <- t, isConExpr base = n <> ": " <> printExpr t val
  | otherwise                       = n <> " = " <> printExpr t val

isConExpr :: Expr -> Bool
isConExpr (Con _)    = True
isConExpr (QCon _ _) = True
isConExpr _          = False

-- Parenthesization helpers

-- | Determine whether an expression needs parentheses in atom position.
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

-- | Print an expression in atom position, adding parentheses if needed.
--
-- Atoms are the tightest binding context: function arguments,
-- record update targets, etc.
printAtom :: Target -> Expr -> Text
printAtom t e
  | isCompound t e = "(" <> printExpr t e <> ")"
  | otherwise      = printExpr t e

-- | Print an expression in function position of an application.
--
-- Like 'printAtom' but allows nested applications without parentheses
-- (since application is left-associative).
printAppFun :: Target -> Expr -> Text
printAppFun t e@(App {}) = printExpr t e  -- left-assoc app doesn't need parens on left
printAppFun t e
  | isCompound t e = "(" <> printExpr t e <> ")"
  | otherwise      = printExpr t e

-- | Print an expression as an argument to an infix operator.
--
-- Parenthesizes nested infix expressions, lambdas, and other constructs
-- that would be ambiguous.
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

-- | Print an expression at infix level (for where\/annotation left-hand sides).
--
-- Parenthesizes lambdas, conditionals, and other non-infix constructs
-- but allows infix expressions and applications through.
printInfixLevel :: Target -> Expr -> Text
printInfixLevel t e@(Lam {})  = "(" <> printExpr t e <> ")"
printInfixLevel t e@(If {})   = "(" <> printExpr t e <> ")"
printInfixLevel t e@(Case {}) = "(" <> printExpr t e <> ")"
printInfixLevel t e@(Let {})  = "(" <> printExpr t e <> ")"
printInfixLevel t e@(Do {})   = "(" <> printExpr t e <> ")"
printInfixLevel t e@(Where {}) = "(" <> printExpr t e <> ")"
printInfixLevel t e@(Ann {})   = "(" <> printExpr t e <> ")"
printInfixLevel t e           = printExpr t e

-- Guard, case alt, binding, and statement printers

-- | Print a single guard condition.
printGuard :: Target -> Guard -> Text
printGuard t (Guard e) = "| " <> printInfixLevel t e

-- | Print a list of guards, or empty text if there are none.
printGuards :: Target -> [Guard] -> Text
printGuards _ [] = ""
printGuards t gs = " " <> T.intercalate " " (map (printGuard t) gs)

-- | Print a case alternative: @pattern -> body@ or @pattern | guard -> body@.
printCaseAlt :: Target -> CaseAlt -> Text
printCaseAlt t (CaseAlt pat guards body) =
  printPat t pat <> printGuards t guards <> " -> " <> printExpr t body

-- | Print a let\/where binding.
--
-- Function-style bindings (@f x = body@) are recognized when the pattern
-- is a 'VarPat' and the body is a 'Lam', and printed in compact form
-- rather than as @f = \\x -> body@.
printBinding :: Target -> Binding -> Text
printBinding t (Binding (VarPat name) (Lam pats body)) =
  printPat t (VarPat name) <> " " <> T.intercalate " " (map (printPatAtom t) pats) <> " = " <> printExpr t body
printBinding t (Binding pat body) = printPat t pat <> " = " <> printExpr t body

-- | Print a do-notation statement.
printStmt :: Target -> Stmt -> Text
printStmt t (StmtBind pat body) = printPat t pat <> " <- " <> printExpr t body
printStmt t (StmtExpr e) = printExpr t e
printStmt t (StmtLet bindings) =
  "let { " <> T.intercalate "; " (map (printBinding t) bindings) <> " }"

-- Type printer

-- | Print a type expression.
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

-- | Print a literal value with proper escaping.
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

-- | Print a pattern for the given target language.
--
-- Handles divergent syntax:
--
-- * __Tuples__: @(a, b)@ (Haskell) vs @Tuple a b@ (PureScript)
-- * __Cons__: @x : xs@ (Haskell) vs @Cons x xs@ (PureScript)
-- * __Record fields__: @=@ separator (Haskell) vs @:@ separator (PureScript)
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
printPat t (RecordPat (Name n) fields) =
  n <> " { " <> T.intercalate ", " (map pf fields) <> " }"
  where pf (Name fn, p) = fn <> sep <> printPat t p
        sep = case t of { PureScript -> ": "; Haskell -> " = " }

-- | Print a pattern in atom position, adding parentheses where needed.
--
-- Constructor patterns with arguments, tuple patterns (in PureScript),
-- cons patterns, as-patterns, negated literals, and record patterns
-- all require parentheses in atom position.
printPatAtom :: Target -> Pat -> Text
printPatAtom t p@(ConPat _ (_:_)) = "(" <> printPat t p <> ")"
printPatAtom t p@(TuplePat _) = case t of
  Haskell    -> printPat t p  -- already parenthesized
  PureScript -> "(" <> printPat t p <> ")"
printPatAtom t p@(ConsPat _ _) = "(" <> printPat t p <> ")"
printPatAtom t p@(AsPat _ _) = "(" <> printPat t p <> ")"
printPatAtom t p@(NegLitPat _) = "(" <> printPat t p <> ")"
printPatAtom t p@(RecordPat _ _) = "(" <> printPat t p <> ")"
printPatAtom t p = printPat t p
