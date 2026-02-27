{-# LANGUAGE OverloadedStrings #-}

module Purekell.ASTSpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Purekell.AST
import Purekell.Arbitrary ()

spec :: Spec
spec = do
  describe "AST" $ do
    describe "Arbitrary Name" $ do
      it "generates non-empty names" $ property $ \(Name n) ->
        not (T.null n)

      it "generates names starting with lowercase" $ property $ \(Name n) ->
        case T.uncons n of
          Just (c, _) -> c `elem` ['a'..'z']
          Nothing -> False

    describe "Arbitrary Lit" $ do
      it "generates valid literals" $ property $ \lit ->
        case lit of
          IntLit n -> n >= 0
          FloatLit d -> d >= 0 && not (isNaN d) && not (isInfinite d)
          StringLit _ -> True
          CharLit _ -> True

    describe "Arbitrary Expr" $ do
      it "generates valid expressions" $ property $ \expr ->
        case (expr :: Expr) of
          Literal _ -> True
          Var _ -> True
          Con _ -> True
          App _ _ -> True
          InfixApp {} -> True
          Lam {} -> True
          If {} -> True
          Case {} -> True
          Let {} -> True
          Do {} -> True
          Neg {} -> True
          RecordAccess {} -> True
          Tuple {} -> True
          ListLit {} -> True
          LeftSection {} -> True
          RightSection {} -> True
          Where {} -> True
