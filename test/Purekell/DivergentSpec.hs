{-# LANGUAGE OverloadedStrings #-}

module Purekell.DivergentSpec (spec) where

import Test.Hspec

import Purekell.AST
import Purekell.Codec (runParse, runPrint)
import Purekell.Haskell (haskellExpr, haskellPat)
import Purekell.PureScript (purescriptExpr, purescriptPat)

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

    describe "Tuple expressions" $ do
      describe "Pair" $ do
        let ast = Tuple [Var (Name "a"), Var (Name "b")]

        it "Haskell prints as (a, b)" $ do
          runPrint haskellExpr ast `shouldBe` "(a, b)"

        it "PureScript prints as Tuple a b" $ do
          runPrint purescriptExpr ast `shouldBe` "Tuple a b"

        it "Haskell parses (a, b) as Tuple" $ do
          runParse haskellExpr "(a, b)" `shouldBe` Right ast

      describe "Triple" $ do
        let ast = Tuple [Var (Name "a"), Var (Name "b"), Var (Name "c")]

        it "Haskell prints as (a, b, c)" $ do
          runPrint haskellExpr ast `shouldBe` "(a, b, c)"

        it "PureScript prints as nested Tuple" $ do
          runPrint purescriptExpr ast `shouldBe` "Tuple a (Tuple b c)"

        it "Haskell parses (a, b, c) as Tuple" $ do
          runParse haskellExpr "(a, b, c)" `shouldBe` Right ast

    describe "Tuple patterns" $ do
      describe "Pair pattern" $ do
        let ast = TuplePat [VarPat (Name "x"), VarPat (Name "y")]

        it "Haskell prints as (x, y)" $ do
          runPrint haskellPat ast `shouldBe` "(x, y)"

        it "PureScript prints as Tuple x y" $ do
          runPrint purescriptPat ast `shouldBe` "Tuple x y"

        it "Haskell parses (x, y) as TuplePat" $ do
          runParse haskellPat "(x, y)" `shouldBe` Right ast

      describe "Triple pattern" $ do
        let ast = TuplePat [VarPat (Name "x"), VarPat (Name "y"), VarPat (Name "z")]

        it "Haskell prints as (x, y, z)" $ do
          runPrint haskellPat ast `shouldBe` "(x, y, z)"

        it "PureScript prints as nested Tuple" $ do
          runPrint purescriptPat ast `shouldBe` "Tuple x (Tuple y z)"

        it "Haskell parses (x, y, z) as TuplePat" $ do
          runParse haskellPat "(x, y, z)" `shouldBe` Right ast

    describe "Nested tuple expressions" $ do
      describe "Tuple as first element" $ do
        let ast = Tuple [Tuple [Var (Name "a"), Var (Name "b")], Var (Name "c")]

        it "Haskell prints as ((a, b), c)" $ do
          runPrint haskellExpr ast `shouldBe` "((a, b), c)"

        it "Haskell parses ((a, b), c)" $ do
          runParse haskellExpr "((a, b), c)" `shouldBe` Right ast

        it "PureScript prints as Tuple (Tuple a b) c" $ do
          runPrint purescriptExpr ast `shouldBe` "Tuple (Tuple a b) c"

      describe "Tuple as second element" $ do
        let ast = Tuple [Var (Name "a"), Tuple [Var (Name "b"), Var (Name "c")]]

        it "Haskell prints as (a, (b, c))" $ do
          runPrint haskellExpr ast `shouldBe` "(a, (b, c))"

        it "Haskell parses (a, (b, c))" $ do
          runParse haskellExpr "(a, (b, c))" `shouldBe` Right ast

        it "PureScript prints as Tuple a (Tuple b c)" $ do
          runPrint purescriptExpr ast `shouldBe` "Tuple a (Tuple b c)"

      describe "Triple vs nested pair" $ do
        -- (a, b, c) and (a, (b, c)) are different ASTs but print the same in PS
        let triple = Tuple [Var (Name "a"), Var (Name "b"), Var (Name "c")]
            nested = Tuple [Var (Name "a"), Tuple [Var (Name "b"), Var (Name "c")]]

        it "PureScript prints both as Tuple a (Tuple b c)" $ do
          runPrint purescriptExpr triple `shouldBe` "Tuple a (Tuple b c)"
          runPrint purescriptExpr nested `shouldBe` "Tuple a (Tuple b c)"

        it "Haskell distinguishes them" $ do
          runPrint haskellExpr triple `shouldBe` "(a, b, c)"
          runPrint haskellExpr nested `shouldBe` "(a, (b, c))"

        it "Haskell roundtrips preserve the distinction" $ do
          runParse haskellExpr "(a, b, c)" `shouldBe` Right triple
          runParse haskellExpr "(a, (b, c))" `shouldBe` Right nested

    describe "Nested tuple patterns" $ do
      describe "Tuple in first position" $ do
        let ast = TuplePat [TuplePat [VarPat (Name "a"), VarPat (Name "b")], VarPat (Name "c")]

        it "Haskell prints as ((a, b), c)" $ do
          runPrint haskellPat ast `shouldBe` "((a, b), c)"

        it "Haskell parses ((a, b), c) pattern" $ do
          runParse haskellPat "((a, b), c)" `shouldBe` Right ast

        it "PureScript prints as Tuple (Tuple a b) c" $ do
          runPrint purescriptPat ast `shouldBe` "Tuple (Tuple a b) c"

      describe "Tuple in second position" $ do
        let ast = TuplePat [VarPat (Name "a"), TuplePat [VarPat (Name "b"), VarPat (Name "c")]]

        it "Haskell prints as (a, (b, c))" $ do
          runPrint haskellPat ast `shouldBe` "(a, (b, c))"

        it "Haskell parses (a, (b, c)) pattern" $ do
          runParse haskellPat "(a, (b, c))" `shouldBe` Right ast

        it "PureScript prints as Tuple a (Tuple b c)" $ do
          runPrint purescriptPat ast `shouldBe` "Tuple a (Tuple b c)"

      describe "Triple vs nested pair patterns" $ do
        let triple = TuplePat [VarPat (Name "a"), VarPat (Name "b"), VarPat (Name "c")]
            nested = TuplePat [VarPat (Name "a"), TuplePat [VarPat (Name "b"), VarPat (Name "c")]]

        it "PureScript prints both as Tuple a (Tuple b c)" $ do
          runPrint purescriptPat triple `shouldBe` "Tuple a (Tuple b c)"
          runPrint purescriptPat nested `shouldBe` "Tuple a (Tuple b c)"

        it "Haskell distinguishes them" $ do
          runPrint haskellPat triple `shouldBe` "(a, b, c)"
          runPrint haskellPat nested `shouldBe` "(a, (b, c))"

        it "Haskell roundtrips preserve the distinction" $ do
          runParse haskellPat "(a, b, c)" `shouldBe` Right triple
          runParse haskellPat "(a, (b, c))" `shouldBe` Right nested
