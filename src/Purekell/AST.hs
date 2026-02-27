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
  deriving (Eq, Show, Generic)

data Pat
  = VarPat Name
  | ConPat Name [Pat]
  | LitPat Lit
  | WildPat
  deriving (Eq, Show, Generic)
