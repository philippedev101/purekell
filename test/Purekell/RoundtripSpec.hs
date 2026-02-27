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

      it "Haskell pat -> PureScript pat -> Haskell pat" $ property $ \pat ->
        let hsText = runPrint haskellPat (pat :: Pat)
        in case runParse purescriptPat hsText of
             Left err -> counterexample (show err) False
             Right psPat ->
               let psText = runPrint purescriptPat psPat
               in case runParse haskellPat psText of
                    Left err -> counterexample (show err) False
                    Right hsPat -> hsPat === pat
