{-# LANGUAGE OverloadedStrings #-}

module Purekell.RoundtripSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Purekell.AST
import Purekell.Arbitrary (noRecordAccess)
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

      it "expr roundtrips" $ property $ \expr ->
        roundtrip purescriptExpr expr === Right (expr :: Expr)

      it "pat roundtrips" $ property $ \pat ->
        roundtrip purescriptPat pat === Right (pat :: Pat)

    describe "Cross-language" $ do
      it "Haskell expr -> PureScript expr -> Haskell expr" $ property $ forAll (arbitrary `suchThat` noRecordAccess) $ \expr ->
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

    describe "Cross-language" $ do
      it "Haskell pat -> PureScript pat -> Haskell pat" $ property $ \pat ->
        let hsText = runPrint haskellPat (pat :: Pat)
        in case runParse purescriptPat hsText of
             Left err -> counterexample (show err) False
             Right psPat ->
               let psText = runPrint purescriptPat psPat
               in case runParse haskellPat psText of
                    Left err -> counterexample (show err) False
                    Right hsPat -> hsPat === pat
