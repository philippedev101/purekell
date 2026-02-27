{-# LANGUAGE OverloadedStrings #-}

module Purekell.RoundtripSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Purekell.AST
import Purekell.Arbitrary (noRecordAccess, noTuple, noTuplePat)
import Purekell.Codec (roundtrip, runParse, runPrint)
import Purekell.Haskell (haskellExpr, haskellLit, haskellPat)
import Purekell.PureScript (purescriptExpr, purescriptLit, purescriptPat)

spec :: Spec
spec = do
  describe "Roundtrip" $ do
    describe "Haskell" $ do
      it "lit roundtrips" $ property $ \lit ->
        roundtrip haskellLit lit === Right (lit :: Lit)

      it "expr roundtrips" $ property $ forAll (arbitrary `suchThat` noRecordAccess) $ \expr ->
        roundtrip haskellExpr expr === Right expr

      it "pat roundtrips" $ property $ \pat ->
        roundtrip haskellPat pat === Right (pat :: Pat)

    describe "PureScript" $ do
      it "lit roundtrips" $ property $ \lit ->
        roundtrip purescriptLit lit === Right (lit :: Lit)

      it "expr roundtrips" $ property $ forAll (arbitrary `suchThat` noTuple) $ \expr ->
        roundtrip purescriptExpr expr === Right (expr :: Expr)

      it "pat roundtrips" $ property $ forAll (arbitrary `suchThat` noTuplePat) $ \pat ->
        roundtrip purescriptPat pat === Right (pat :: Pat)

    describe "Cross-language" $ do
      it "Haskell expr -> PureScript expr -> Haskell expr" $ property $ forAll (arbitrary `suchThat` (\e -> noRecordAccess e && noTuple e)) $ \expr ->
        let hsText = runPrint haskellExpr expr
        in case runParse purescriptExpr hsText of
             Left err -> counterexample (show err) False
             Right psExpr ->
               let psText = runPrint purescriptExpr psExpr
               in case runParse haskellExpr psText of
                    Left err -> counterexample (show err) False
                    Right hsExpr -> hsExpr === expr

    describe "Negation" $ do
      it "parses -x" $
        runParse haskellExpr "-x" `shouldBe` Right (Neg (Var (Name "x")))

      it "parses -42" $
        runParse haskellExpr "-42" `shouldBe` Right (Neg (Literal (IntLit 42)))

      it "parses -f x" $
        runParse haskellExpr "-f x" `shouldBe` Right (Neg (App (Var (Name "f")) (Var (Name "x"))))

      it "parses -x + y" $
        runParse haskellExpr "-x + y" `shouldBe` Right (InfixApp (Neg (Var (Name "x"))) (Name "+") (Var (Name "y")))

      it "parses x + -y" $
        runParse haskellExpr "x + -y" `shouldBe` Right (InfixApp (Var (Name "x")) (Name "+") (Neg (Var (Name "y"))))

      it "prints -x" $
        runPrint haskellExpr (Neg (Var (Name "x"))) `shouldBe` "-x"

      it "prints -(a + b)" $
        runPrint haskellExpr (Neg (InfixApp (Var (Name "a")) (Name "+") (Var (Name "b")))) `shouldBe` "-(a + b)"

      it "prints -(-x)" $
        runPrint haskellExpr (Neg (Neg (Var (Name "x")))) `shouldBe` "-(-x)"

      it "roundtrips negation via PureScript" $
        runParse purescriptExpr (runPrint purescriptExpr (Neg (Var (Name "x")))) `shouldBe` Right (Neg (Var (Name "x")))

      it "does not treat -> as negation" $
        runParse haskellExpr "\\x -> x" `shouldBe` Right (Lam [VarPat (Name "x")] (Var (Name "x")))

    describe "Float literals" $ do
      it "parses 3.14 as FloatLit" $
        runParse haskellExpr "3.14" `shouldBe` Right (Literal (FloatLit 3.14))

      it "parses 0.5 as FloatLit" $
        runParse haskellExpr "0.5" `shouldBe` Right (Literal (FloatLit 0.5))

      it "parses 1.0e-2 (scientific notation)" $
        runParse haskellExpr "1.0e-2" `shouldBe` Right (Literal (FloatLit 1.0e-2))

      it "42 remains IntLit (not FloatLit)" $
        runParse haskellExpr "42" `shouldBe` Right (Literal (IntLit 42))

      it "prints FloatLit 3.14" $
        runPrint haskellExpr (Literal (FloatLit 3.14)) `shouldBe` "3.14"

      it "parses -3.14 as negated float" $
        runParse haskellExpr "-3.14" `shouldBe` Right (Neg (Literal (FloatLit 3.14)))

      it "parses x + 3.14" $
        runParse haskellExpr "x + 3.14" `shouldBe` Right (InfixApp (Var (Name "x")) (Name "+") (Literal (FloatLit 3.14)))

      it "PureScript parses 3.14" $
        runParse purescriptExpr "3.14" `shouldBe` Right (Literal (FloatLit 3.14))

      it "PureScript prints FloatLit 3.14" $
        runPrint purescriptExpr (Literal (FloatLit 3.14)) `shouldBe` "3.14"

      it "float literal in pattern" $
        runParse haskellExpr "case x of { 3.14 -> y }" `shouldBe`
          Right (Case (Var (Name "x")) [CaseAlt (LitPat (FloatLit 3.14)) [] (Var (Name "y"))])

    describe "Tuple in context" $ do
      it "parses \\(x, y) -> x" $
        runParse haskellExpr "\\(x, y) -> x" `shouldBe`
          Right (Lam [TuplePat [VarPat (Name "x"), VarPat (Name "y")]] (Var (Name "x")))

      it "prints \\(x, y) -> x" $
        runPrint haskellExpr (Lam [TuplePat [VarPat (Name "x"), VarPat (Name "y")]] (Var (Name "x")))
          `shouldBe` "\\(x, y) -> x"

      it "parses case p of { (a, b) -> a }" $
        runParse haskellExpr "case p of { (a, b) -> a }" `shouldBe`
          Right (Case (Var (Name "p"))
            [CaseAlt (TuplePat [VarPat (Name "a"), VarPat (Name "b")]) [] (Var (Name "a"))])

      it "prints case with tuple pattern" $
        runPrint haskellExpr (Case (Var (Name "p"))
            [CaseAlt (TuplePat [VarPat (Name "a"), VarPat (Name "b")]) [] (Var (Name "a"))])
          `shouldBe` "case p of { (a, b) -> a }"

      it "roundtrips \\(x, y) -> x + y" $
        let ast = Lam [TuplePat [VarPat (Name "x"), VarPat (Name "y")]]
                    (InfixApp (Var (Name "x")) (Name "+") (Var (Name "y")))
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "roundtrips case with tuple pattern" $
        let ast = Case (Var (Name "p"))
                    [CaseAlt (TuplePat [VarPat (Name "a"), VarPat (Name "b")]) [] (Var (Name "a"))]
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "parses complex expressions inside tuple" $
        runParse haskellExpr "(a + b, f c)" `shouldBe`
          Right (Tuple
            [ InfixApp (Var (Name "a")) (Name "+") (Var (Name "b"))
            , App (Var (Name "f")) (Var (Name "c"))
            ])

      it "prints complex expressions inside tuple" $
        runPrint haskellExpr (Tuple
            [ InfixApp (Var (Name "a")) (Name "+") (Var (Name "b"))
            , App (Var (Name "f")) (Var (Name "c"))
            ])
          `shouldBe` "(a + b, f c)"

      it "roundtrips complex expressions inside tuple" $
        let ast = Tuple
              [ InfixApp (Var (Name "a")) (Name "+") (Var (Name "b"))
              , App (Var (Name "f")) (Var (Name "c"))
              ]
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "Haskell tuple as function argument" $
        let ast = App (Var (Name "f")) (Tuple [Var (Name "a"), Var (Name "b")])
        in do
          runPrint haskellExpr ast `shouldBe` "f (a, b)"
          roundtrip haskellExpr ast `shouldBe` Right ast

      it "PureScript tuple as function argument gets parens" $
        let ast = App (Var (Name "f")) (Tuple [Var (Name "a"), Var (Name "b")])
        in runPrint purescriptExpr ast `shouldBe` "f (Tuple a b)"

    describe "Cross-language" $ do
      it "Haskell pat -> PureScript pat -> Haskell pat" $ property $ forAll (arbitrary `suchThat` noTuplePat) $ \pat ->
        let hsText = runPrint haskellPat (pat :: Pat)
        in case runParse purescriptPat hsText of
             Left err -> counterexample (show err) False
             Right psPat ->
               let psText = runPrint purescriptPat psPat
               in case runParse haskellPat psText of
                    Left err -> counterexample (show err) False
                    Right hsPat -> hsPat === pat
