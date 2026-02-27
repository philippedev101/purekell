{-# LANGUAGE OverloadedStrings #-}

module Purekell.InstanceSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Purekell.AST
import Purekell.Arbitrary (noRecordAccess, noTuple, noTuplePat)
import Purekell.Instance

spec :: Spec
spec = do
  describe "Instance" $ do
    describe "parseMethodBody" $ do
      it "parses simple equality method" $ do
        let input = "eq x y = x == y"
        let expected = [MethodEquation
              (Name "eq")
              [VarPat (Name "x"), VarPat (Name "y")]
              []
              (InfixApp (Var (Name "x")) (Name "==") (Var (Name "y")))]
        parseMethodBody input `shouldBe` Right expected

      it "parses method with pattern matching on constructors" $ do
        let input = "compare (MkId x) (MkId y) = compare x y"
        let expected = [MethodEquation
              (Name "compare")
              [ConPat (Name "MkId") [VarPat (Name "x")], ConPat (Name "MkId") [VarPat (Name "y")]]
              []
              (App (App (Var (Name "compare")) (Var (Name "x"))) (Var (Name "y")))]
        parseMethodBody input `shouldBe` Right expected

      it "parses method with case expression" $ do
        let input = "show x = case x of { MkId i -> show i }"
        let expected = [MethodEquation
              (Name "show")
              [VarPat (Name "x")]
              []
              (Case (Var (Name "x"))
                [CaseAlt (ConPat (Name "MkId") [VarPat (Name "i")])
                  []
                  (App (Var (Name "show")) (Var (Name "i")))])]
        parseMethodBody input `shouldBe` Right expected

      it "parses multiple equations separated by semicolons" $ do
        let input = "eq (Left x) (Left y) = eq x y; eq (Right x) (Right y) = eq x y; eq _ _ = False"
        case parseMethodBody input of
          Left err -> expectationFailure (show err)
          Right eqs -> length eqs `shouldBe` 3

    describe "printMethodBody" $ do
      it "prints for Haskell" $ do
        let eq = MethodEquation (Name "eq") [VarPat (Name "x"), VarPat (Name "y")] []
                   (InfixApp (Var (Name "x")) (Name "==") (Var (Name "y")))
        printMethodBody Haskell [eq] `shouldBe` "eq x y = x == y"

      it "prints for PureScript" $ do
        let eq = MethodEquation (Name "eq") [VarPat (Name "x"), VarPat (Name "y")] []
                   (InfixApp (Var (Name "x")) (Name "==") (Var (Name "y")))
        printMethodBody PureScript [eq] `shouldBe` "eq x y = x == y"

      it "prints record access differently per target" $ do
        let eq = MethodEquation (Name "eq") [VarPat (Name "x"), VarPat (Name "y")] []
                   (InfixApp
                     (RecordAccess (Var (Name "x")) (Name "uid"))
                     (Name "==")
                     (RecordAccess (Var (Name "y")) (Name "uid")))
        printMethodBody Haskell [eq] `shouldBe` "eq x y = uid x == uid y"
        printMethodBody PureScript [eq] `shouldBe` "eq x y = x.uid == y.uid"

    describe "hsToPs golden tests" $ do
      it "DataID Eq instance body" $ do
        let hsBody = "eq (MkDataID x) (MkDataID y) = x == y"
        case parseMethodBody hsBody of
          Left err -> expectationFailure (show err)
          Right eqs -> do
            printMethodBody Haskell eqs `shouldBe` "eq (MkDataID x) (MkDataID y) = x == y"
            printMethodBody PureScript eqs `shouldBe` "eq (MkDataID x) (MkDataID y) = x == y"

      it "Show instance with string concatenation" $ do
        let hsBody = "show (MkId x) = \"MkId \" <> show x"
        case parseMethodBody hsBody of
          Left err -> expectationFailure (show err)
          Right eqs -> do
            -- Same syntax in both languages with <>
            printMethodBody PureScript eqs `shouldBe` "show (MkId x) = \"MkId \" <> show x"

    describe "Roundtrip" $ do
      let noRA (MethodEquation _ _ gs body) =
            noRecordAccess body && all (\(Guard e) -> noRecordAccess e) gs
      let noPsTuple (MethodEquation _ pats gs body) =
            noRA (MethodEquation (Name "") [] gs body)
            && noTuple body && all (\(Guard e) -> noTuple e) gs
            && all noTuplePat pats
      it "Haskell printMethodBody roundtrips" $ property $
        forAll (arbitrary `suchThat` noRA) $ \eq ->
          parseMethodBody (printMethodBody Haskell [eq]) === Right [eq :: MethodEquation]

      it "PureScript printMethodBody roundtrips" $ property $
        forAll (arbitrary `suchThat` noPsTuple) $ \eq ->
          parseMethodBody (printMethodBody PureScript [eq]) === Right [eq :: MethodEquation]
