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

      describe "4-tuple" $ do
        let ast = Tuple [Var (Name "a"), Var (Name "b"), Var (Name "c"), Var (Name "d")]

        it "Haskell prints as (a, b, c, d)" $ do
          runPrint haskellExpr ast `shouldBe` "(a, b, c, d)"

        it "PureScript prints as nested Tuple" $ do
          runPrint purescriptExpr ast `shouldBe` "Tuple a (Tuple b (Tuple c d))"

        it "Haskell roundtrips" $ do
          runParse haskellExpr "(a, b, c, d)" `shouldBe` Right ast

      describe "5-tuple" $ do
        let ast = Tuple [Var (Name "a"), Var (Name "b"), Var (Name "c"), Var (Name "d"), Var (Name "e")]

        it "Haskell prints as (a, b, c, d, e)" $ do
          runPrint haskellExpr ast `shouldBe` "(a, b, c, d, e)"

        it "PureScript prints as nested Tuple" $ do
          runPrint purescriptExpr ast `shouldBe` "Tuple a (Tuple b (Tuple c (Tuple d e)))"

        it "Haskell roundtrips" $ do
          runParse haskellExpr "(a, b, c, d, e)" `shouldBe` Right ast

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

      describe "4-tuple pattern" $ do
        let ast = TuplePat [VarPat (Name "a"), VarPat (Name "b"), VarPat (Name "c"), VarPat (Name "d")]

        it "Haskell prints as (a, b, c, d)" $ do
          runPrint haskellPat ast `shouldBe` "(a, b, c, d)"

        it "PureScript prints as nested Tuple" $ do
          runPrint purescriptPat ast `shouldBe` "Tuple a (Tuple b (Tuple c d))"

        it "Haskell roundtrips" $ do
          runParse haskellPat "(a, b, c, d)" `shouldBe` Right ast

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

    describe "Cons patterns" $ do
      describe "Simple cons" $ do
        let ast = ConsPat (VarPat (Name "x")) (VarPat (Name "xs"))

        it "Haskell prints as x : xs" $ do
          runPrint haskellPat ast `shouldBe` "x : xs"

        it "PureScript prints as Cons x xs" $ do
          runPrint purescriptPat ast `shouldBe` "Cons x xs"

        it "Haskell parses x : xs" $ do
          runParse haskellPat "x : xs" `shouldBe` Right ast

      describe "Nested cons" $ do
        let ast = ConsPat (VarPat (Name "x")) (ConsPat (VarPat (Name "y")) (VarPat (Name "zs")))

        it "Haskell prints as x : y : zs" $ do
          runPrint haskellPat ast `shouldBe` "x : y : zs"

        it "PureScript prints as Cons x (Cons y zs)" $ do
          runPrint purescriptPat ast `shouldBe` "Cons x (Cons y zs)"

      describe "Cons with constructor" $ do
        let ast = ConsPat (ConPat (Name "Just") [VarPat (Name "x")]) (VarPat (Name "xs"))

        it "Haskell prints as (Just x) : xs" $ do
          runPrint haskellPat ast `shouldBe` "(Just x) : xs"

        it "PureScript prints as Cons (Just x) xs" $ do
          runPrint purescriptPat ast `shouldBe` "Cons (Just x) xs"

      describe "Cross-language one-way" $ do
        it "PS Cons x xs parses as ConPat, not ConsPat" $ do
          let ast = ConsPat (VarPat (Name "x")) (VarPat (Name "xs"))
              psText = runPrint purescriptPat ast
          psText `shouldBe` "Cons x xs"
          runParse purescriptPat psText `shouldBe`
            Right (ConPat (Name "Cons") [VarPat (Name "x"), VarPat (Name "xs")])

    describe "Record construction" $ do
      let ast = RecordUpdate (Con (Name "MkFoo")) [(Name "bar", Literal (IntLit 1))]

      it "Haskell prints with = separator" $ do
        runPrint haskellExpr ast `shouldBe` "MkFoo { bar = 1 }"

      it "PureScript prints with : separator" $ do
        runPrint purescriptExpr ast `shouldBe` "MkFoo { bar: 1 }"

      it "Haskell roundtrips" $ do
        runParse haskellExpr "MkFoo { bar = 1 }" `shouldBe` Right ast

      it "PureScript roundtrips" $ do
        runParse purescriptExpr "MkFoo { bar: 1 }" `shouldBe` Right ast

      it "cross-language roundtrip" $ do
        let hsText = runPrint haskellExpr ast
        let psText = runPrint purescriptExpr ast
        runParse purescriptExpr hsText `shouldBe` Right ast
        runParse haskellExpr psText `shouldBe` Right ast

    describe "Record construction with QCon" $ do
      let ast = RecordUpdate (QCon [Name "Data", Name "Foo"] (Name "MkBar")) [(Name "x", Literal (IntLit 1))]

      it "Haskell prints with = separator" $ do
        runPrint haskellExpr ast `shouldBe` "Data.Foo.MkBar { x = 1 }"

      it "PureScript prints with : separator" $ do
        runPrint purescriptExpr ast `shouldBe` "Data.Foo.MkBar { x: 1 }"

    describe "Record update (non-constructor base)" $ do
      let ast = RecordUpdate (Var (Name "rec")) [(Name "bar", Literal (IntLit 1))]

      it "Haskell prints with = separator" $ do
        runPrint haskellExpr ast `shouldBe` "rec { bar = 1 }"

      it "PureScript also prints with = separator" $ do
        runPrint purescriptExpr ast `shouldBe` "rec { bar = 1 }"

    describe "Record patterns" $ do
      let ast = RecordPat (Name "Foo") [(Name "bar", VarPat (Name "x"))]

      it "Haskell prints with = separator" $ do
        runPrint haskellPat ast `shouldBe` "Foo { bar = x }"

      it "PureScript prints with : separator" $ do
        runPrint purescriptPat ast `shouldBe` "Foo { bar: x }"

      it "Haskell roundtrips" $ do
        runParse haskellPat "Foo { bar = x }" `shouldBe` Right ast

      it "PureScript roundtrips" $ do
        runParse purescriptPat "Foo { bar: x }" `shouldBe` Right ast

      it "cross-language roundtrip" $ do
        let hsText = runPrint haskellPat ast
        let psText = runPrint purescriptPat ast
        runParse purescriptPat hsText `shouldBe` Right ast
        runParse haskellPat psText `shouldBe` Right ast

    describe "Multi-field record pattern" $ do
      let ast = RecordPat (Name "Foo") [(Name "bar", VarPat (Name "x")), (Name "baz", VarPat (Name "y"))]

      it "Haskell prints with = separator" $ do
        runPrint haskellPat ast `shouldBe` "Foo { bar = x, baz = y }"

      it "PureScript prints with : separator" $ do
        runPrint purescriptPat ast `shouldBe` "Foo { bar: x, baz: y }"

      it "both roundtrip" $ do
        runParse haskellPat "Foo { bar = x, baz = y }" `shouldBe` Right ast
        runParse purescriptPat "Foo { bar: x, baz: y }" `shouldBe` Right ast

    describe "Tuple cross-language one-way translation" $ do
      it "HS Tuple -> PS text -> PS parse gives App/Con, not Tuple" $ do
        let ast = Tuple [Var (Name "a"), Var (Name "b")]
            psText = runPrint purescriptExpr ast
        psText `shouldBe` "Tuple a b"
        -- Parsing this back gives constructor application, not Tuple node
        runParse purescriptExpr psText `shouldBe`
          Right (App (App (Con (Name "Tuple")) (Var (Name "a"))) (Var (Name "b")))

      it "HS TuplePat -> PS text -> PS parse gives ConPat, not TuplePat" $ do
        let ast = TuplePat [VarPat (Name "x"), VarPat (Name "y")]
            psText = runPrint purescriptPat ast
        psText `shouldBe` "Tuple x y"
        -- Parsing this back gives constructor pattern, not TuplePat node
        runParse purescriptPat psText `shouldBe`
          Right (ConPat (Name "Tuple") [VarPat (Name "x"), VarPat (Name "y")])
