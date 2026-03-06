-- | The shared abstract syntax tree for Haskell and PureScript expressions.
--
-- This AST is language-neutral: it represents the common subset of both
-- languages plus nodes for constructs that exist in both but with different
-- concrete syntax. For example, 'RecordAccess' represents @rec.field@ in
-- PureScript and @field rec@ in Haskell — same AST node, different surface
-- syntax handled by the printer.
--
-- All AST types derive 'Generic' for use with property-based testing
-- (arbitrary instance generation).
module Purekell.AST
  ( -- * Names
    Name (..)
    -- * Literals
  , Lit (..)
    -- * Expressions
  , Expr (..)
    -- * Patterns
  , Pat (..)
    -- * Guards, alternatives, bindings, statements
  , Guard (..)
  , CaseAlt (..)
  , Binding (..)
  , Stmt (..)
    -- * Types
  , Type (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | An identifier or operator name. Wraps a 'Text' value.
newtype Name = Name Text
  deriving (Eq, Show, Ord, Generic)

-- | Literal values shared by both languages.
data Lit
  = IntLit Integer       -- ^ Integer literal: @42@
  | FloatLit Double      -- ^ Floating-point literal: @3.14@
  | StringLit Text       -- ^ String literal: @\"hello\"@
  | CharLit Char         -- ^ Character literal: @\'x\'@
  deriving (Eq, Show, Generic)

-- | A guard expression: @| condition@.
--
-- Used in case alternatives and method equations.
newtype Guard = Guard Expr
  deriving (Eq, Show, Generic)

-- | A case alternative: @pattern guards -> body@.
--
-- The guard list may be empty for unconditional alternatives.
data CaseAlt = CaseAlt Pat [Guard] Expr
  deriving (Eq, Show, Generic)

-- | A let\/where binding: @pattern = expression@.
--
-- Function-style bindings like @f x = body@ are represented as
-- @'Binding' ('VarPat' \"f\") ('Lam' ['VarPat' \"x\"] body)@.
data Binding = Binding Pat Expr
  deriving (Eq, Show, Generic)

-- | A do-notation statement.
data Stmt
  = StmtBind Pat Expr    -- ^ Bind statement: @pat <- expr@
  | StmtExpr Expr        -- ^ Expression statement: @expr@
  | StmtLet [Binding]    -- ^ Let statement: @let { bindings }@
  deriving (Eq, Show, Generic)

-- | Expressions — the core of the AST.
--
-- Covers the shared expression syntax of Haskell and PureScript:
-- function application, infix operators, lambdas, conditionals,
-- case expressions, let\/where bindings, do-notation, record operations,
-- sections, type annotations, and more.
data Expr
  = Literal Lit                       -- ^ A literal value
  | Var Name                          -- ^ Variable reference: @x@, @foo@
  | Con Name                          -- ^ Data constructor: @Just@, @True@
  | App Expr Expr                     -- ^ Function application: @f x@
  | InfixApp Expr Name Expr           -- ^ Infix operator application: @x + y@, @x \`div\` y@
  | Lam [Pat] Expr                    -- ^ Lambda: @\\x y -> body@
  | If Expr Expr Expr                 -- ^ Conditional: @if c then t else e@
  | Case Expr [CaseAlt]              -- ^ Case expression: @case x of { alts }@
  | Let [Binding] Expr               -- ^ Let expression: @let { bindings } in body@
  | Do [Stmt]                         -- ^ Do-notation: @do { stmts }@
  | Neg Expr                          -- ^ Prefix negation: @-expr@
  | RecordAccess Expr Name            -- ^ Record field access: @rec.field@ (PS) \/ @field rec@ (HS)
  | Tuple [Expr]                      -- ^ Tuple literal (≥2 elements): @(a, b)@ (HS) \/ @Tuple a b@ (PS)
  | ListLit [Expr]                    -- ^ List literal: @[1, 2, 3]@
  | LeftSection Expr Name             -- ^ Left operator section: @(expr +)@
  | RightSection Name Expr            -- ^ Right operator section: @(+ expr)@
  | Where Expr [Binding]             -- ^ Where clause: @expr where { bindings }@
  | Ann Expr Type                     -- ^ Type annotation: @expr :: Type@
  | RecordUpdate Expr [(Name, Expr)] -- ^ Record update: @rec { field = val, ... }@
  | QVar [Name] Name                  -- ^ Qualified variable: @Data.Map.lookup@
  | QCon [Name] Name                  -- ^ Qualified constructor: @Data.Map.Map@
  deriving (Eq, Show, Generic)

-- | Patterns for case alternatives, lambda arguments, and bindings.
data Pat
  = VarPat Name                       -- ^ Variable pattern: @x@
  | ConPat Name [Pat]                 -- ^ Constructor pattern: @Just x@, @Nothing@
  | LitPat Lit                        -- ^ Literal pattern: @42@, @\"hello\"@
  | WildPat                           -- ^ Wildcard pattern: @_@
  | TuplePat [Pat]                    -- ^ Tuple pattern (≥2 elements): @(a, b)@ (HS) \/ @Tuple a b@ (PS)
  | ListPat [Pat]                     -- ^ List pattern: @[x, y, z]@
  | ConsPat Pat Pat                   -- ^ Cons pattern: @x : xs@ (HS) \/ @Cons x xs@ (PS)
  | AsPat Name Pat                    -- ^ As-pattern: @name\@pat@
  | NegLitPat Lit                     -- ^ Negated literal pattern: @-42@, @-3.14@
  | RecordPat Name [(Name, Pat)]     -- ^ Record pattern: @Foo { bar = x, baz = y }@
  deriving (Eq, Show, Generic)

-- | Types for type annotations (@expr :: Type@).
--
-- Only a minimal subset is supported — enough for type annotations
-- that appear in instance method bodies.
data Type
  = TyCon Name                        -- ^ Named type: @Int@, @Bool@, @Maybe@
  | TyVar Name                        -- ^ Type variable: @a@, @b@
  | TyApp Type Type                   -- ^ Type application: @Maybe a@
  | TyFun Type Type                   -- ^ Function type: @a -> b@
  | TyQCon [Name] Name               -- ^ Qualified type constructor: @Data.Map.Map@
  deriving (Eq, Show, Generic)
