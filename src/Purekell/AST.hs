{-# LANGUAGE DeriveGeneric #-}

module Purekell.AST
  ( Name (..)
  , Lit (..)
  , Expr (..)
  , Pat (..)
  , Guard (..)
  , CaseAlt (..)
  , Binding (..)
  , Stmt (..)
  , Type (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

newtype Name = Name Text
  deriving (Eq, Show, Ord, Generic)

data Lit
  = IntLit Integer
  | FloatLit Double
  | StringLit Text
  | CharLit Char
  deriving (Eq, Show, Generic)

newtype Guard = Guard Expr
  deriving (Eq, Show, Generic)

data CaseAlt = CaseAlt Pat [Guard] Expr
  deriving (Eq, Show, Generic)

data Binding = Binding Pat Expr
  deriving (Eq, Show, Generic)

data Stmt
  = StmtBind Pat Expr
  | StmtExpr Expr
  | StmtLet [Binding]
  deriving (Eq, Show, Generic)

data Expr
  = Literal Lit
  | Var Name
  | Con Name
  | App Expr Expr
  | InfixApp Expr Name Expr
  | Lam [Pat] Expr
  | If Expr Expr Expr
  | Case Expr [CaseAlt]
  | Let [Binding] Expr
  | Do [Stmt]
  | Neg Expr               -- ^ Prefix negation: -expr
  | RecordAccess Expr Name  -- ^ Record field access: rec.field (PS) / field rec (HS)
  | Tuple [Expr]            -- ^ Tuple literal (≥2 elements)
  | ListLit [Expr]          -- ^ List literal: [1, 2, 3]
  | LeftSection Expr Name   -- ^ Left operator section: (expr op)
  | RightSection Name Expr  -- ^ Right operator section: (op expr)
  | Where Expr [Binding]    -- ^ Where clause: expr where { pat = expr; ... }
  | Ann Expr Type           -- ^ Type annotation: expr :: Type
  | RecordUpdate Expr [(Name, Expr)]  -- ^ Record update: rec { field = val, ... }
  | QVar [Name] Name   -- ^ Qualified variable: Data.Map.lookup
  | QCon [Name] Name   -- ^ Qualified constructor: Data.Map.Map
  deriving (Eq, Show, Generic)

data Pat
  = VarPat Name
  | ConPat Name [Pat]
  | LitPat Lit
  | WildPat
  | TuplePat [Pat]          -- ^ Tuple pattern (≥2 elements)
  | ListPat [Pat]           -- ^ List pattern: [x, y, z]
  | ConsPat Pat Pat          -- ^ Cons pattern: x : xs (HS) / Cons x xs (PS)
  | AsPat Name Pat           -- ^ As-pattern: name@pat
  | NegLitPat Lit            -- ^ Negated literal pattern: -42, -3.14
  | RecordPat Name [(Name, Pat)]  -- ^ Record pattern: Foo { bar = x, baz = y }
  deriving (Eq, Show, Generic)

data Type
  = TyCon Name           -- ^ Named type: Int, Bool, Maybe
  | TyVar Name           -- ^ Type variable: a, b
  | TyApp Type Type      -- ^ Type application: Maybe a
  | TyFun Type Type      -- ^ Function type: a -> b
  | TyQCon [Name] Name  -- ^ Qualified type constructor: Data.Map.Map
  deriving (Eq, Show, Generic)
