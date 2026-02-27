{-# LANGUAGE OverloadedStrings #-}

module Purekell.DivergentSpec (spec) where

import Test.Hspec

import Purekell.AST
import Purekell.Codec (runParse, runPrint)
import Purekell.Haskell (haskellExpr)
import Purekell.PureScript (purescriptExpr)

spec :: Spec
spec = do
  describe "Divergent constructs" $ do
    describe "Record access" $ do
      let ast = RecordAccess (Var (Name "rec")) (Name "field")

      it "Haskell prints as function application: field rec" $ do
        runPrint haskellExpr ast `shouldBe` "field rec"

      it "PureScript prints as dot access: rec.field" $ do
        runPrint purescriptExpr ast `shouldBe` "rec.field"

      it "PureScript parses dot access" $ do
        runParse purescriptExpr "rec.field" `shouldBe` Right ast

      it "Haskell -> PureScript translation works for RecordAccess" $ do
        -- The Haskell output "field rec" is App (Var field) (Var rec)
        -- when parsed by Haskell; but the AST RecordAccess translates
        -- to different syntax in each language
        let hsText = runPrint haskellExpr ast
        hsText `shouldBe` "field rec"
        let psText = runPrint purescriptExpr ast
        psText `shouldBe` "rec.field"

    describe "Chained record access" $ do
      let ast = RecordAccess (RecordAccess (Var (Name "rec")) (Name "inner")) (Name "field")

      it "PureScript prints chained dots" $ do
        runPrint purescriptExpr ast `shouldBe` "rec.inner.field"

      it "PureScript parses chained dots" $ do
        runParse purescriptExpr "rec.inner.field" `shouldBe` Right ast

      it "Haskell prints as nested application" $ do
        runPrint haskellExpr ast `shouldBe` "field (inner rec)"
