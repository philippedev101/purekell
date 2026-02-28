{-# LANGUAGE OverloadedStrings #-}

module Purekell.InstanceSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Purekell.AST
import Purekell.Arbitrary (noRecordAccess, noTuple, noTuplePat, noConsExpr, noConsPat)
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

    describe "Tuple in instance methods" $ do
      it "Haskell prints tuple in method body" $ do
        let eq = MethodEquation (Name "toPair") [ConPat (Name "MkT") [VarPat (Name "x"), VarPat (Name "y")]] []
                   (Tuple [Var (Name "x"), Var (Name "y")])
        printMethodBody Haskell [eq] `shouldBe` "toPair (MkT x y) = (x, y)"

      it "PureScript prints tuple in method body" $ do
        let eq = MethodEquation (Name "toPair") [ConPat (Name "MkT") [VarPat (Name "x"), VarPat (Name "y")]] []
                   (Tuple [Var (Name "x"), Var (Name "y")])
        printMethodBody PureScript [eq] `shouldBe` "toPair (MkT x y) = Tuple x y"

      it "Haskell prints tuple pattern in method args" $ do
        let eq = MethodEquation (Name "fst'") [TuplePat [VarPat (Name "a"), VarPat (Name "b")]] []
                   (Var (Name "a"))
        printMethodBody Haskell [eq] `shouldBe` "fst' (a, b) = a"

      it "PureScript prints tuple pattern in method args" $ do
        let eq = MethodEquation (Name "fst'") [TuplePat [VarPat (Name "a"), VarPat (Name "b")]] []
                   (Var (Name "a"))
        printMethodBody PureScript [eq] `shouldBe` "fst' (Tuple a b) = a"

      it "Haskell tuple in method body roundtrips" $ do
        let input = "toPair (MkT x y) = (x, y)"
        case parseMethodBody input of
          Left err -> expectationFailure (show err)
          Right eqs -> printMethodBody Haskell eqs `shouldBe` input

      it "Haskell tuple pattern in method args roundtrips" $ do
        let input = "fst' (a, b) = a"
        case parseMethodBody input of
          Left err -> expectationFailure (show err)
          Right eqs -> printMethodBody Haskell eqs `shouldBe` input

    describe "Cons pattern in instance methods" $ do
      it "Haskell prints cons pattern in method args" $ do
        let eq = MethodEquation (Name "head'") [ConsPat (VarPat (Name "x")) WildPat] []
                   (Var (Name "x"))
        printMethodBody Haskell [eq] `shouldBe` "head' (x : _) = x"

      it "PureScript prints cons pattern in method args" $ do
        let eq = MethodEquation (Name "head'") [ConsPat (VarPat (Name "x")) WildPat] []
                   (Var (Name "x"))
        printMethodBody PureScript [eq] `shouldBe` "head' (Cons x _) = x"

    describe "List in instance methods" $ do
      it "list pattern in method args" $ do
        let eq = MethodEquation (Name "single") [ListPat [VarPat (Name "x")]] []
                   (Var (Name "x"))
        printMethodBody Haskell [eq] `shouldBe` "single [x] = x"
        printMethodBody PureScript [eq] `shouldBe` "single [x] = x"

      it "list literal in method body" $ do
        let eq = MethodEquation (Name "wrap") [VarPat (Name "x")] []
                   (ListLit [Var (Name "x")])
        printMethodBody Haskell [eq] `shouldBe` "wrap x = [x]"
        printMethodBody PureScript [eq] `shouldBe` "wrap x = [x]"

    describe "As-pattern in method args" $ do
      it "Haskell prints as-pattern in method args" $ do
        let eq = MethodEquation (Name "head'") [AsPat (Name "xs") (ConsPat (VarPat (Name "x")) WildPat)] []
                   (Var (Name "x"))
        printMethodBody Haskell [eq] `shouldBe` "head' (xs@(x : _)) = x"
        printMethodBody PureScript [eq] `shouldBe` "head' (xs@(Cons x _)) = x"

      it "Haskell as-pattern roundtrips via parseMethodBody" $ do
        let input = "head' (xs@(x : _)) = x"
        case parseMethodBody input of
          Left err -> expectationFailure (show err)
          Right eqs -> printMethodBody Haskell eqs `shouldBe` input

    describe "Negated literal in method" $ do
      it "prints and roundtrips negated literal pattern" $ do
        let eq = MethodEquation (Name "isNegOne") [NegLitPat (IntLit 1)] []
                   (Con (Name "True"))
        printMethodBody Haskell [eq] `shouldBe` "isNegOne (-1) = True"
        printMethodBody PureScript [eq] `shouldBe` "isNegOne (-1) = True"

      it "Haskell negated literal roundtrips via parseMethodBody" $ do
        let input = "isNegOne (-1) = True"
        case parseMethodBody input of
          Left err -> expectationFailure (show err)
          Right eqs -> printMethodBody Haskell eqs `shouldBe` input

    describe "Where clause in method body" $ do
      it "prints and roundtrips where in method body" $ do
        let input = "f x = y where { y = x + 1 }"
        case parseMethodBody input of
          Left err -> expectationFailure (show err)
          Right eqs -> printMethodBody Haskell eqs `shouldBe` input

      it "where with multiple bindings in method body" $ do
        let input = "f x = a + b where { a = x; b = 1 }"
        case parseMethodBody input of
          Left err -> expectationFailure (show err)
          Right eqs -> printMethodBody Haskell eqs `shouldBe` input

    describe "Type annotation in method body" $ do
      it "prints and roundtrips type annotation in method body" $ do
        let input = "f x = x :: Int"
        case parseMethodBody input of
          Left err -> expectationFailure (show err)
          Right eqs -> printMethodBody Haskell eqs `shouldBe` input

      it "prints type annotation for PureScript" $ do
        let eq = MethodEquation (Name "f") [VarPat (Name "x")] []
                   (Ann (Var (Name "x")) (TyCon (Name "Int")))
        printMethodBody PureScript [eq] `shouldBe` "f x = x :: Int"

    describe "Record update in method body" $ do
      it "prints and roundtrips record update in method body" $ do
        let input = "f x = x { y = 1 }"
        case parseMethodBody input of
          Left err -> expectationFailure (show err)
          Right eqs -> printMethodBody Haskell eqs `shouldBe` input

      it "prints record update for PureScript" $ do
        let eq = MethodEquation (Name "f") [VarPat (Name "x")] []
                   (RecordUpdate (Var (Name "x")) [(Name "y", Literal (IntLit 1))])
        printMethodBody PureScript [eq] `shouldBe` "f x = x { y = 1 }"

    describe "Qualified names in methods" $ do
      it "method with qualified variable" $ do
        let input = "f x = Data.Map.lookup x m"
        case parseMethodBody input of
          Left err -> expectationFailure (show err)
          Right eqs -> do
            printMethodBody Haskell eqs `shouldBe` input
            printMethodBody PureScript eqs `shouldBe` input

      it "method with qualified constructor" $ do
        let input = "f x = Data.Map.Map x"
        case parseMethodBody input of
          Left err -> expectationFailure (show err)
          Right eqs -> do
            printMethodBody Haskell eqs `shouldBe` input
            printMethodBody PureScript eqs `shouldBe` input

    describe "Record pattern in method" $ do
      it "method with record pattern" $ do
        let input = "f (Foo { bar = x }) = x"
        case parseMethodBody input of
          Left err -> expectationFailure (show err)
          Right eqs -> do
            printMethodBody Haskell eqs `shouldBe` input
            printMethodBody PureScript eqs `shouldBe` "f (Foo { bar: x }) = x"

      it "method with record construction" $ do
        let eq = MethodEquation (Name "f") [VarPat (Name "x")] []
                   (RecordUpdate (Con (Name "MkFoo")) [(Name "bar", Var (Name "x"))])
        printMethodBody Haskell [eq] `shouldBe` "f x = MkFoo { bar = x }"
        printMethodBody PureScript [eq] `shouldBe` "f x = MkFoo { bar: x }"

    describe "Roundtrip" $ do
      let noRA (MethodEquation _ _ gs body) =
            noRecordAccess body && all (\(Guard e) -> noRecordAccess e) gs
      let noPsTuple (MethodEquation _ pats gs body) =
            noRA (MethodEquation (Name "") [] gs body)
            && noTuple body && all (\(Guard e) -> noTuple e) gs
            && all noTuplePat pats
            && noConsExpr body && all (\(Guard e) -> noConsExpr e) gs
            && all noConsPat pats
      it "Haskell printMethodBody roundtrips" $ property $
        forAll (arbitrary `suchThat` noRA) $ \eq ->
          parseMethodBody (printMethodBody Haskell [eq]) === Right [eq :: MethodEquation]

      it "PureScript printMethodBody roundtrips" $ property $
        forAll (arbitrary `suchThat` noPsTuple) $ \eq ->
          parseMethodBody (printMethodBody PureScript [eq]) === Right [eq :: MethodEquation]

    describe "Backtick operator in method body" $ do
      it "method body with backtick operator" $ do
        let input = "f x y = x `div` y"
        case parseMethodBody input of
          Left err -> expectationFailure (show err)
          Right eqs -> do
            printMethodBody Haskell eqs `shouldBe` input
            printMethodBody PureScript eqs `shouldBe` input

    describe "Function binding in method where" $ do
      it "method body with where containing function binding" $ do
        let input = "f x = g x where { g y = y + 1 }"
        case parseMethodBody input of
          Left err -> expectationFailure (show err)
          Right eqs -> printMethodBody Haskell eqs `shouldBe` input
