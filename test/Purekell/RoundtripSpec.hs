{-# LANGUAGE OverloadedStrings #-}

module Purekell.RoundtripSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Purekell.AST
import Purekell.Arbitrary (noRecordAccess, noTuple, noTuplePat, noConsExpr, noConsPat, noQualClash)
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

      it "expr roundtrips" $ property $ forAll (arbitrary `suchThat` (\e -> noTuple e && noConsExpr e && noQualClash e)) $ \expr ->
        roundtrip purescriptExpr expr === Right (expr :: Expr)

      it "pat roundtrips" $ property $ forAll (arbitrary `suchThat` (\p -> noTuplePat p && noConsPat p)) $ \pat ->
        roundtrip purescriptPat pat === Right (pat :: Pat)

    describe "Cross-language" $ do
      it "Haskell expr -> PureScript expr -> Haskell expr" $ property $ forAll (arbitrary `suchThat` (\e -> noRecordAccess e && noTuple e && noConsExpr e)) $ \expr ->
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
      it "Haskell pat -> PureScript pat -> Haskell pat" $ property $ forAll (arbitrary `suchThat` (\p -> noTuplePat p && noConsPat p)) $ \pat ->
        let hsText = runPrint haskellPat (pat :: Pat)
        in case runParse purescriptPat hsText of
             Left err -> counterexample (show err) False
             Right psPat ->
               let psText = runPrint purescriptPat psPat
               in case runParse haskellPat psText of
                    Left err -> counterexample (show err) False
                    Right hsPat -> hsPat === pat

    describe "List literals" $ do
      it "parses []" $
        runParse haskellExpr "[]" `shouldBe` Right (ListLit [])

      it "parses [x]" $
        runParse haskellExpr "[x]" `shouldBe` Right (ListLit [Var (Name "x")])

      it "parses [1, 2, 3]" $
        runParse haskellExpr "[1, 2, 3]" `shouldBe`
          Right (ListLit [Literal (IntLit 1), Literal (IntLit 2), Literal (IntLit 3)])

      it "parses [a + b, f c]" $
        runParse haskellExpr "[a + b, f c]" `shouldBe`
          Right (ListLit
            [ InfixApp (Var (Name "a")) (Name "+") (Var (Name "b"))
            , App (Var (Name "f")) (Var (Name "c"))
            ])

      it "parses [[1], [2]]" $
        runParse haskellExpr "[[1], [2]]" `shouldBe`
          Right (ListLit [ListLit [Literal (IntLit 1)], ListLit [Literal (IntLit 2)]])

      it "prints []" $
        runPrint haskellExpr (ListLit []) `shouldBe` "[]"

      it "prints [1, 2, 3]" $
        runPrint haskellExpr (ListLit [Literal (IntLit 1), Literal (IntLit 2), Literal (IntLit 3)])
          `shouldBe` "[1, 2, 3]"

      it "roundtrips [] in Haskell" $
        roundtrip haskellExpr (ListLit []) `shouldBe` Right (ListLit [])

      it "roundtrips [x] in Haskell" $
        roundtrip haskellExpr (ListLit [Var (Name "x")]) `shouldBe` Right (ListLit [Var (Name "x")])

      it "roundtrips [1, 2, 3] in Haskell" $
        let ast = ListLit [Literal (IntLit 1), Literal (IntLit 2), Literal (IntLit 3)]
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "roundtrips in PureScript" $
        let ast = ListLit [Var (Name "x"), Var (Name "y")]
        in roundtrip purescriptExpr ast `shouldBe` Right ast

      it "list literal as function argument" $
        let ast = App (Var (Name "f")) (ListLit [Var (Name "a"), Var (Name "b")])
        in do
          runPrint haskellExpr ast `shouldBe` "f [a, b]"
          roundtrip haskellExpr ast `shouldBe` Right ast

      it "PureScript prints list same as Haskell" $
        runPrint purescriptExpr (ListLit [Literal (IntLit 1), Literal (IntLit 2)])
          `shouldBe` "[1, 2]"

      it "cross-language roundtrips" $
        let ast = ListLit [Literal (IntLit 1), Literal (IntLit 2)]
            hsText = runPrint haskellExpr ast
        in case runParse purescriptExpr hsText of
             Left err -> expectationFailure (show err)
             Right psExpr ->
               let psText = runPrint purescriptExpr psExpr
               in runParse haskellExpr psText `shouldBe` Right ast

    describe "List patterns" $ do
      it "parses []" $
        runParse haskellPat "[]" `shouldBe` Right (ListPat [])

      it "parses [x]" $
        runParse haskellPat "[x]" `shouldBe` Right (ListPat [VarPat (Name "x")])

      it "parses [x, y, z]" $
        runParse haskellPat "[x, y, z]" `shouldBe`
          Right (ListPat [VarPat (Name "x"), VarPat (Name "y"), VarPat (Name "z")])

      it "list in case" $
        runParse haskellExpr "case xs of { [] -> 0; [x] -> x }" `shouldBe`
          Right (Case (Var (Name "xs"))
            [ CaseAlt (ListPat []) [] (Literal (IntLit 0))
            , CaseAlt (ListPat [VarPat (Name "x")]) [] (Var (Name "x"))
            ])

      it "list in lambda" $
        runParse haskellExpr "\\[x] -> x" `shouldBe`
          Right (Lam [ListPat [VarPat (Name "x")]] (Var (Name "x")))

      it "roundtrips [] pattern in Haskell" $
        roundtrip haskellPat (ListPat []) `shouldBe` Right (ListPat [])

      it "roundtrips [x, y] pattern in Haskell" $
        let ast = ListPat [VarPat (Name "x"), VarPat (Name "y")]
        in roundtrip haskellPat ast `shouldBe` Right ast

      it "roundtrips list pattern in PureScript" $
        let ast = ListPat [VarPat (Name "x"), VarPat (Name "y")]
        in roundtrip purescriptPat ast `shouldBe` Right ast

      it "cross-language roundtrips" $
        let ast = ListPat [VarPat (Name "x"), VarPat (Name "y")]
            hsText = runPrint haskellPat ast
        in case runParse purescriptPat hsText of
             Left err -> expectationFailure (show err)
             Right psPat ->
               let psText = runPrint purescriptPat psPat
               in runParse haskellPat psText `shouldBe` Right ast

    describe "Cons patterns" $ do
      it "parses x : xs" $
        runParse haskellPat "x : xs" `shouldBe`
          Right (ConsPat (VarPat (Name "x")) (VarPat (Name "xs")))

      it "parses x : y : zs (right-associative)" $
        runParse haskellPat "x : y : zs" `shouldBe`
          Right (ConsPat (VarPat (Name "x")) (ConsPat (VarPat (Name "y")) (VarPat (Name "zs"))))

      it "parses (Just x) : xs" $
        runParse haskellPat "(Just x) : xs" `shouldBe`
          Right (ConsPat (ConPat (Name "Just") [VarPat (Name "x")]) (VarPat (Name "xs")))

      it "Haskell prints x : xs" $
        runPrint haskellPat (ConsPat (VarPat (Name "x")) (VarPat (Name "xs")))
          `shouldBe` "x : xs"

      it "Haskell prints x : y : zs" $
        runPrint haskellPat (ConsPat (VarPat (Name "x")) (ConsPat (VarPat (Name "y")) (VarPat (Name "zs"))))
          `shouldBe` "x : y : zs"

      it "roundtrips x : xs in Haskell" $
        let ast = ConsPat (VarPat (Name "x")) (VarPat (Name "xs"))
        in roundtrip haskellPat ast `shouldBe` Right ast

      it "roundtrips x : y : zs in Haskell" $
        let ast = ConsPat (VarPat (Name "x")) (ConsPat (VarPat (Name "y")) (VarPat (Name "zs")))
        in roundtrip haskellPat ast `shouldBe` Right ast

      it "cons in case" $
        runParse haskellExpr "case xs of { x : rest -> x; [] -> 0 }" `shouldBe`
          Right (Case (Var (Name "xs"))
            [ CaseAlt (ConsPat (VarPat (Name "x")) (VarPat (Name "rest"))) [] (Var (Name "x"))
            , CaseAlt (ListPat []) [] (Literal (IntLit 0))
            ])

      it "roundtrips cons in case" $
        let ast = Case (Var (Name "xs"))
              [ CaseAlt (ConsPat (VarPat (Name "x")) (VarPat (Name "rest"))) [] (Var (Name "x"))
              , CaseAlt (ListPat []) [] (Literal (IntLit 0))
              ]
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "Just x : xs (constructor then cons)" $
        runParse haskellPat "Just x : xs" `shouldBe`
          Right (ConsPat (ConPat (Name "Just") [VarPat (Name "x")]) (VarPat (Name "xs")))

    describe "Operator sections" $ do
      it "parses (+ 1)" $
        runParse haskellExpr "(+ 1)" `shouldBe`
          Right (RightSection (Name "+") (Literal (IntLit 1)))

      it "parses (1 +)" $
        runParse haskellExpr "(1 +)" `shouldBe`
          Right (LeftSection (Literal (IntLit 1)) (Name "+"))

      it "parses (== 0)" $
        runParse haskellExpr "(== 0)" `shouldBe`
          Right (RightSection (Name "==") (Literal (IntLit 0)))

      it "parses (<> \"x\")" $
        runParse haskellExpr "(<> \"x\")" `shouldBe`
          Right (RightSection (Name "<>") (Literal (StringLit "x")))

      it "(-x) is NOT a section (still negation)" $
        runParse haskellExpr "(-x)" `shouldBe`
          Right (Neg (Var (Name "x")))

      it "section as argument: map (+ 1) xs" $
        runParse haskellExpr "map (+ 1) xs" `shouldBe`
          Right (App (App (Var (Name "map")) (RightSection (Name "+") (Literal (IntLit 1)))) (Var (Name "xs")))

      it "left section with app: (f x +)" $
        runParse haskellExpr "(f x +)" `shouldBe`
          Right (LeftSection (App (Var (Name "f")) (Var (Name "x"))) (Name "+"))

      it "(a + b) is grouped infix, NOT a section" $
        runParse haskellExpr "(a + b)" `shouldBe`
          Right (InfixApp (Var (Name "a")) (Name "+") (Var (Name "b")))

      it "prints (+ 1)" $
        runPrint haskellExpr (RightSection (Name "+") (Literal (IntLit 1)))
          `shouldBe` "(+ 1)"

      it "prints (1 +)" $
        runPrint haskellExpr (LeftSection (Literal (IntLit 1)) (Name "+"))
          `shouldBe` "(1 +)"

      it "roundtrips (+ 1) in Haskell" $
        let ast = RightSection (Name "+") (Literal (IntLit 1))
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "roundtrips (1 +) in Haskell" $
        let ast = LeftSection (Literal (IntLit 1)) (Name "+")
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "roundtrips (+ 1) in PureScript" $
        let ast = RightSection (Name "+") (Literal (IntLit 1))
        in roundtrip purescriptExpr ast `shouldBe` Right ast

      it "roundtrips (1 +) in PureScript" $
        let ast = LeftSection (Literal (IntLit 1)) (Name "+")
        in roundtrip purescriptExpr ast `shouldBe` Right ast

      it "PureScript prints (+ 1) same as Haskell" $
        runPrint purescriptExpr (RightSection (Name "+") (Literal (IntLit 1)))
          `shouldBe` "(+ 1)"

      it "PureScript prints (1 +) same as Haskell" $
        runPrint purescriptExpr (LeftSection (Literal (IntLit 1)) (Name "+"))
          `shouldBe` "(1 +)"

      it "cross-language roundtrips" $
        let ast = LeftSection (Var (Name "x")) (Name "+")
            hsText = runPrint haskellExpr ast
        in case runParse purescriptExpr hsText of
             Left err -> expectationFailure (show err)
             Right psExpr ->
               let psText = runPrint purescriptExpr psExpr
               in runParse haskellExpr psText `shouldBe` Right ast

    describe "As-patterns" $ do
      it "parses x@(Just y)" $
        runParse haskellPat "x@(Just y)" `shouldBe`
          Right (AsPat (Name "x") (ConPat (Name "Just") [VarPat (Name "y")]))

      it "parses xs@[]" $
        runParse haskellPat "xs@[]" `shouldBe`
          Right (AsPat (Name "xs") (ListPat []))

      it "parses xs@(a : b)" $
        runParse haskellPat "xs@(a : b)" `shouldBe`
          Right (AsPat (Name "xs") (ConsPat (VarPat (Name "a")) (VarPat (Name "b"))))

      it "prints AsPat in Haskell" $
        runPrint haskellPat (AsPat (Name "x") (ConPat (Name "Just") [VarPat (Name "y")]))
          `shouldBe` "x@(Just y)"

      it "prints AsPat in PureScript" $
        runPrint purescriptPat (AsPat (Name "x") (ConPat (Name "Just") [VarPat (Name "y")]))
          `shouldBe` "x@(Just y)"

      it "roundtrips x@(Just y) in Haskell" $
        let ast = AsPat (Name "x") (ConPat (Name "Just") [VarPat (Name "y")])
        in roundtrip haskellPat ast `shouldBe` Right ast

      it "roundtrips xs@[a, b] in Haskell" $
        let ast = AsPat (Name "xs") (ListPat [VarPat (Name "a"), VarPat (Name "b")])
        in roundtrip haskellPat ast `shouldBe` Right ast

      it "roundtrips x@(Just y) in PureScript" $
        let ast = AsPat (Name "x") (ConPat (Name "Just") [VarPat (Name "y")])
        in roundtrip purescriptPat ast `shouldBe` Right ast

      it "cross-language roundtrips" $
        let ast = AsPat (Name "x") (ConPat (Name "Just") [VarPat (Name "y")])
            hsText = runPrint haskellPat ast
        in case runParse purescriptPat hsText of
             Left err -> expectationFailure (show err)
             Right psPat ->
               let psText = runPrint purescriptPat psPat
               in runParse haskellPat psText `shouldBe` Right ast

      it "in case" $
        runParse haskellExpr "case xs of { ys@(x : _) -> ys; _ -> [] }" `shouldBe`
          Right (Case (Var (Name "xs"))
            [ CaseAlt (AsPat (Name "ys") (ConsPat (VarPat (Name "x")) WildPat)) [] (Var (Name "ys"))
            , CaseAlt WildPat [] (ListLit [])
            ])

      it "in lambda" $
        runParse haskellExpr "\\xs@(x : _) -> x" `shouldBe`
          Right (Lam [AsPat (Name "xs") (ConsPat (VarPat (Name "x")) WildPat)] (Var (Name "x")))

    describe "Negated literal patterns" $ do
      it "parses -1" $
        runParse haskellPat "-1" `shouldBe` Right (NegLitPat (IntLit 1))

      it "parses -3.14" $
        runParse haskellPat "-3.14" `shouldBe` Right (NegLitPat (FloatLit 3.14))

      it "prints NegLitPat" $
        runPrint haskellPat (NegLitPat (IntLit 1)) `shouldBe` "-1"

      it "prints NegLitPat in PureScript" $
        runPrint purescriptPat (NegLitPat (FloatLit 3.14)) `shouldBe` "-3.14"

      it "prints NegLitPat in constructor (gets parens)" $
        runPrint haskellPat (ConPat (Name "Just") [NegLitPat (IntLit 1)])
          `shouldBe` "Just (-1)"

      it "roundtrips Just (-1) in Haskell" $
        let ast = ConPat (Name "Just") [NegLitPat (IntLit 1)]
        in roundtrip haskellPat ast `shouldBe` Right ast

      it "roundtrips -1 in Haskell" $
        roundtrip haskellPat (NegLitPat (IntLit 1)) `shouldBe` Right (NegLitPat (IntLit 1))

      it "roundtrips -3.14 in Haskell" $
        roundtrip haskellPat (NegLitPat (FloatLit 3.14)) `shouldBe` Right (NegLitPat (FloatLit 3.14))

      it "roundtrips -1 in PureScript" $
        roundtrip purescriptPat (NegLitPat (IntLit 1)) `shouldBe` Right (NegLitPat (IntLit 1))

      it "cross-language roundtrips" $
        let ast = NegLitPat (IntLit 42)
            hsText = runPrint haskellPat ast
        in case runParse purescriptPat hsText of
             Left err -> expectationFailure (show err)
             Right psPat ->
               let psText = runPrint purescriptPat psPat
               in runParse haskellPat psText `shouldBe` Right ast

      it "in case" $
        runParse haskellExpr "case x of { -1 -> True; _ -> False }" `shouldBe`
          Right (Case (Var (Name "x"))
            [ CaseAlt (NegLitPat (IntLit 1)) [] (Con (Name "True"))
            , CaseAlt WildPat [] (Con (Name "False"))
            ])

      it "parenthesized (-1) as pattern" $
        runParse haskellPat "(-1)" `shouldBe` Right (NegLitPat (IntLit 1))

    describe "Where clauses" $ do
      it "parses x + 1 where { x = 42 }" $
        runParse haskellExpr "x + 1 where { x = 42 }" `shouldBe`
          Right (Where
            (InfixApp (Var (Name "x")) (Name "+") (Literal (IntLit 1)))
            [Binding (VarPat (Name "x")) (Literal (IntLit 42))])

      it "parses y where { y = f x; x = 3 }" $
        runParse haskellExpr "y where { y = f x; x = 3 }" `shouldBe`
          Right (Where
            (Var (Name "y"))
            [ Binding (VarPat (Name "y")) (App (Var (Name "f")) (Var (Name "x")))
            , Binding (VarPat (Name "x")) (Literal (IntLit 3))
            ])

      it "prints Where" $
        runPrint haskellExpr (Where
            (InfixApp (Var (Name "x")) (Name "+") (Literal (IntLit 1)))
            [Binding (VarPat (Name "x")) (Literal (IntLit 42))])
          `shouldBe` "x + 1 where { x = 42 }"

      it "roundtrips x + 1 where { x = 42 } in Haskell" $
        let ast = Where
              (InfixApp (Var (Name "x")) (Name "+") (Literal (IntLit 1)))
              [Binding (VarPat (Name "x")) (Literal (IntLit 42))]
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "roundtrips in PureScript" $
        let ast = Where
              (InfixApp (Var (Name "x")) (Name "+") (Literal (IntLit 1)))
              [Binding (VarPat (Name "x")) (Literal (IntLit 42))]
        in roundtrip purescriptExpr ast `shouldBe` Right ast

      it "cross-language roundtrips" $
        let ast = Where (Var (Name "y")) [Binding (VarPat (Name "y")) (Literal (IntLit 1))]
            hsText = runPrint haskellExpr ast
        in case runParse purescriptExpr hsText of
             Left err -> expectationFailure (show err)
             Right psExpr ->
               let psText = runPrint purescriptExpr psExpr
               in runParse haskellExpr psText `shouldBe` Right ast

      it "where in lambda body" $
        runParse haskellExpr "\\x -> y where { y = x + 1 }" `shouldBe`
          Right (Lam [VarPat (Name "x")]
            (Where (Var (Name "y"))
              [Binding (VarPat (Name "y")) (InfixApp (Var (Name "x")) (Name "+") (Literal (IntLit 1)))]))

      it "where in case alt body" $
        runParse haskellExpr "case n of { 0 -> 1; _ -> n * f n where { f = fac } }" `shouldBe`
          Right (Case (Var (Name "n"))
            [ CaseAlt (LitPat (IntLit 0)) [] (Literal (IntLit 1))
            , CaseAlt WildPat []
                (Where (InfixApp (Var (Name "n")) (Name "*") (App (Var (Name "f")) (Var (Name "n"))))
                  [Binding (VarPat (Name "f")) (Var (Name "fac"))])
            ])

      it "nested where" $
        runParse haskellExpr "x where { x = y where { y = 1 } }" `shouldBe`
          Right (Where (Var (Name "x"))
            [Binding (VarPat (Name "x"))
              (Where (Var (Name "y"))
                [Binding (VarPat (Name "y")) (Literal (IntLit 1))])])

      it "where as function argument needs parens" $
        let ast = App (Var (Name "f")) (Where (Var (Name "x")) [Binding (VarPat (Name "x")) (Literal (IntLit 1))])
        in do
          runPrint haskellExpr ast `shouldBe` "f (x where { x = 1 })"
          roundtrip haskellExpr ast `shouldBe` Right ast

      it "where with multiple bindings" $
        let ast = Where
              (InfixApp (Var (Name "x")) (Name "+") (Var (Name "y")))
              [ Binding (VarPat (Name "x")) (Literal (IntLit 1))
              , Binding (VarPat (Name "y")) (Literal (IntLit 2))
              ]
        in roundtrip haskellExpr ast `shouldBe` Right ast

    describe "Type annotations" $ do
      it "parses x :: Int" $
        runParse haskellExpr "x :: Int" `shouldBe`
          Right (Ann (Var (Name "x")) (TyCon (Name "Int")))

      it "parses f x :: Maybe Int" $
        runParse haskellExpr "f x :: Maybe Int" `shouldBe`
          Right (Ann (App (Var (Name "f")) (Var (Name "x"))) (TyApp (TyCon (Name "Maybe")) (TyCon (Name "Int"))))

      it "parses x :: a -> b" $
        runParse haskellExpr "x :: a -> b" `shouldBe`
          Right (Ann (Var (Name "x")) (TyFun (TyVar (Name "a")) (TyVar (Name "b"))))

      it "prints Ann" $
        runPrint haskellExpr (Ann (Var (Name "x")) (TyCon (Name "Int")))
          `shouldBe` "x :: Int"

      it "prints Ann as function argument (gets parens)" $
        let ast = App (Var (Name "f")) (Ann (Var (Name "x")) (TyCon (Name "Int")))
        in runPrint haskellExpr ast `shouldBe` "f (x :: Int)"

      it "roundtrips x :: Int in Haskell" $
        let ast = Ann (Var (Name "x")) (TyCon (Name "Int"))
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "roundtrips (x + y) :: Int in Haskell" $
        let ast = Ann (InfixApp (Var (Name "x")) (Name "+") (Var (Name "y"))) (TyCon (Name "Int"))
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "roundtrips x :: Int in PureScript" $
        let ast = Ann (Var (Name "x")) (TyCon (Name "Int"))
        in roundtrip purescriptExpr ast `shouldBe` Right ast

      it "cross-language roundtrips" $
        let ast = Ann (Var (Name "x")) (TyCon (Name "Int"))
            hsText = runPrint haskellExpr ast
        in case runParse purescriptExpr hsText of
             Left err -> expectationFailure (show err)
             Right psExpr ->
               let psText = runPrint purescriptExpr psExpr
               in runParse haskellExpr psText `shouldBe` Right ast

      it "in lambda body: \\x -> x :: Int" $
        runParse haskellExpr "\\x -> x :: Int" `shouldBe`
          Right (Lam [VarPat (Name "x")] (Ann (Var (Name "x")) (TyCon (Name "Int"))))

      it "nested: (x :: Int) :: Int" $
        runParse haskellExpr "(x :: Int) :: Int" `shouldBe`
          Right (Ann (Ann (Var (Name "x")) (TyCon (Name "Int"))) (TyCon (Name "Int")))

      it "type with function arrow: f :: Int -> Bool" $
        let ast = Ann (Var (Name "f")) (TyFun (TyCon (Name "Int")) (TyCon (Name "Bool")))
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "type application: x :: Maybe Int" $
        let ast = Ann (Var (Name "x")) (TyApp (TyCon (Name "Maybe")) (TyCon (Name "Int")))
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "parenthesized type: f :: (a -> b) -> c" $
        let ast = Ann (Var (Name "f")) (TyFun (TyFun (TyVar (Name "a")) (TyVar (Name "b"))) (TyVar (Name "c")))
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "in case scrutinee: case (x :: Int) of { ... }" $
        runParse haskellExpr "case (x :: Int) of { _ -> y }" `shouldBe`
          Right (Case (Ann (Var (Name "x")) (TyCon (Name "Int")))
            [CaseAlt WildPat [] (Var (Name "y"))])

      it "annotation then where: x :: Int where { y = 1 }" $
        runParse haskellExpr "x :: Int where { y = 1 }" `shouldBe`
          Right (Where (Ann (Var (Name "x")) (TyCon (Name "Int")))
            [Binding (VarPat (Name "y")) (Literal (IntLit 1))])

      it ":: is not an operator" $
        runParse haskellExpr "x :: Int" `shouldBe`
          Right (Ann (Var (Name "x")) (TyCon (Name "Int")))

    describe "Record updates" $ do
      it "parses rec { x = 1 }" $
        runParse haskellExpr "rec { x = 1 }" `shouldBe`
          Right (RecordUpdate (Var (Name "rec")) [(Name "x", Literal (IntLit 1))])

      it "parses rec { x = 1, y = 2 }" $
        runParse haskellExpr "rec { x = 1, y = 2 }" `shouldBe`
          Right (RecordUpdate (Var (Name "rec"))
            [ (Name "x", Literal (IntLit 1))
            , (Name "y", Literal (IntLit 2))
            ])

      it "prints RecordUpdate" $
        runPrint haskellExpr (RecordUpdate (Var (Name "rec")) [(Name "x", Literal (IntLit 1))])
          `shouldBe` "rec { x = 1 }"

      it "roundtrips rec { x = 1 } in Haskell" $
        let ast = RecordUpdate (Var (Name "rec")) [(Name "x", Literal (IntLit 1))]
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "roundtrips rec { x = 1, y = 2 } in Haskell" $
        let ast = RecordUpdate (Var (Name "rec"))
              [ (Name "x", Literal (IntLit 1))
              , (Name "y", Literal (IntLit 2))
              ]
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "roundtrips rec { x = 1 } in PureScript" $
        let ast = RecordUpdate (Var (Name "rec")) [(Name "x", Literal (IntLit 1))]
        in roundtrip purescriptExpr ast `shouldBe` Right ast

      it "cross-language roundtrips" $
        let ast = RecordUpdate (Var (Name "rec")) [(Name "x", Literal (IntLit 1))]
            hsText = runPrint haskellExpr ast
        in case runParse purescriptExpr hsText of
             Left err -> expectationFailure (show err)
             Right psExpr ->
               let psText = runPrint purescriptExpr psExpr
               in runParse haskellExpr psText `shouldBe` Right ast

      it "as function argument: f rec { x = 1 } parses as f (RecordUpdate rec ...)" $
        runParse haskellExpr "f rec { x = 1 }" `shouldBe`
          Right (App (Var (Name "f")) (RecordUpdate (Var (Name "rec")) [(Name "x", Literal (IntLit 1))]))

      it "with parenthesized base: (f x) { y = 1 }" $
        let ast = RecordUpdate (App (Var (Name "f")) (Var (Name "x"))) [(Name "y", Literal (IntLit 1))]
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "complex field values: rec { x = a + b }" $
        let ast = RecordUpdate (Var (Name "rec"))
              [(Name "x", InfixApp (Var (Name "a")) (Name "+") (Var (Name "b")))]
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "nested: rec { x = inner { y = 1 } }" $
        let ast = RecordUpdate (Var (Name "rec"))
              [(Name "x", RecordUpdate (Var (Name "inner")) [(Name "y", Literal (IntLit 1))])]
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "in infix: rec { x = 1 } + y" $
        runParse haskellExpr "rec { x = 1 } + y" `shouldBe`
          Right (InfixApp
            (RecordUpdate (Var (Name "rec")) [(Name "x", Literal (IntLit 1))])
            (Name "+")
            (Var (Name "y")))

      it "chained record updates: rec { x = 1 } { y = 2 }" $
        let ast = RecordUpdate (RecordUpdate (Var (Name "rec")) [(Name "x", Literal (IntLit 1))])
              [(Name "y", Literal (IntLit 2))]
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "in case scrutinee: case rec { x = 1 } of { ... }" $
        runParse haskellExpr "case rec { x = 1 } of { _ -> y }" `shouldBe`
          Right (Case (RecordUpdate (Var (Name "rec")) [(Name "x", Literal (IntLit 1))])
            [CaseAlt WildPat [] (Var (Name "y"))])

    describe "Qualified names" $ do
      -- Parse tests
      it "parses Data.Map.lookup as QVar" $
        runParse haskellExpr "Data.Map.lookup" `shouldBe`
          Right (QVar [Name "Data", Name "Map"] (Name "lookup"))

      it "parses Data.Map.Map as QCon" $
        runParse haskellExpr "Data.Map.Map" `shouldBe`
          Right (QCon [Name "Data", Name "Map"] (Name "Map"))

      it "parses Foo.bar as QVar" $
        runParse haskellExpr "Foo.bar" `shouldBe`
          Right (QVar [Name "Foo"] (Name "bar"))

      it "parses Foo.Bar as QCon" $
        runParse haskellExpr "Foo.Bar" `shouldBe`
          Right (QCon [Name "Foo"] (Name "Bar"))

      it "parses Foo as Con (unchanged)" $
        runParse haskellExpr "Foo" `shouldBe`
          Right (Con (Name "Foo"))

      -- Print tests
      it "prints QVar" $
        runPrint haskellExpr (QVar [Name "Data", Name "Map"] (Name "lookup"))
          `shouldBe` "Data.Map.lookup"

      it "prints QCon" $
        runPrint haskellExpr (QCon [Name "Data", Name "Map"] (Name "Map"))
          `shouldBe` "Data.Map.Map"

      -- Roundtrip tests
      it "QVar roundtrips in Haskell" $
        let ast = QVar [Name "Data", Name "Map"] (Name "lookup")
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "QCon roundtrips in Haskell" $
        let ast = QCon [Name "Data", Name "Map"] (Name "Map")
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "QVar roundtrips in PureScript" $
        let ast = QVar [Name "Data", Name "Map"] (Name "lookup")
        in roundtrip purescriptExpr ast `shouldBe` Right ast

      it "QCon roundtrips in PureScript" $
        let ast = QCon [Name "Data", Name "Map"] (Name "Map")
        in roundtrip purescriptExpr ast `shouldBe` Right ast

      it "cross-language roundtrips" $
        let ast = QVar [Name "Data", Name "Map"] (Name "lookup")
            hsText = runPrint haskellExpr ast
        in case runParse purescriptExpr hsText of
             Left err -> expectationFailure (show err)
             Right psExpr ->
               let psText = runPrint purescriptExpr psExpr
               in runParse haskellExpr psText `shouldBe` Right ast

      -- Context tests
      it "Data.Map.lookup x parses as App (QVar ...) (Var x)" $
        runParse haskellExpr "Data.Map.lookup x" `shouldBe`
          Right (App (QVar [Name "Data", Name "Map"] (Name "lookup")) (Var (Name "x")))

      it "x :: Data.Map.Map Int String parses with TyQCon" $
        runParse haskellExpr "x :: Data.Map.Map Int String" `shouldBe`
          Right (Ann (Var (Name "x"))
            (TyApp (TyApp (TyQCon [Name "Data", Name "Map"] (Name "Map")) (TyCon (Name "Int"))) (TyCon (Name "String"))))

      it "Foo.bar.baz in PS parses as RecordAccess (QVar ...) baz" $
        runParse purescriptExpr "Foo.bar.baz" `shouldBe`
          Right (RecordAccess (QVar [Name "Foo"] (Name "bar")) (Name "baz"))

      it "rec.field in PS still parses as RecordAccess (Var rec) field" $
        runParse purescriptExpr "rec.field" `shouldBe`
          Right (RecordAccess (Var (Name "rec")) (Name "field"))

      -- Edge cases
      it "Foo . bar (spaces) is NOT qualified — it's infix . operator" $
        runParse haskellExpr "Foo . bar" `shouldBe`
          Right (InfixApp (Con (Name "Foo")) (Name ".") (Var (Name "bar")))

      it "qualified in record update: Foo.Bar { x = 1 }" $
        runParse haskellExpr "Foo.Bar { x = 1 }" `shouldBe`
          Right (RecordUpdate (QCon [Name "Foo"] (Name "Bar")) [(Name "x", Literal (IntLit 1))])

    describe "Record patterns" $ do
      it "parses Foo { bar = x }" $
        runParse haskellPat "Foo { bar = x }" `shouldBe`
          Right (RecordPat (Name "Foo") [(Name "bar", VarPat (Name "x"))])

      it "parses Foo { bar: x } (colon separator)" $
        runParse haskellPat "Foo { bar: x }" `shouldBe`
          Right (RecordPat (Name "Foo") [(Name "bar", VarPat (Name "x"))])

      it "parses MkFoo { bar: 1 } as RecordUpdate (colon in expression)" $
        runParse haskellExpr "MkFoo { bar: 1 }" `shouldBe`
          Right (RecordUpdate (Con (Name "MkFoo")) [(Name "bar", Literal (IntLit 1))])

      it "multi-field record pattern roundtrips in Haskell" $
        let ast = RecordPat (Name "Foo") [(Name "bar", VarPat (Name "x")), (Name "baz", VarPat (Name "y"))]
        in roundtrip haskellPat ast `shouldBe` Right ast

      it "record pattern inside parenthesized context" $
        runParse haskellPat "(Foo { bar = x })" `shouldBe`
          Right (RecordPat (Name "Foo") [(Name "bar", VarPat (Name "x"))])

      it "record construction roundtrips in Haskell" $
        let ast = RecordUpdate (Con (Name "MkFoo")) [(Name "bar", Literal (IntLit 1))]
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "record construction roundtrips in PureScript" $
        let ast = RecordUpdate (Con (Name "MkFoo")) [(Name "bar", Literal (IntLit 1))]
        in roundtrip purescriptExpr ast `shouldBe` Right ast

      it "cross-language roundtrip for record construction" $
        let ast = RecordUpdate (Con (Name "MkFoo")) [(Name "bar", Literal (IntLit 1))]
            hsText = runPrint haskellExpr ast
        in case runParse purescriptExpr hsText of
             Left err -> expectationFailure (show err)
             Right psExpr ->
               let psText = runPrint purescriptExpr psExpr
               in runParse haskellExpr psText `shouldBe` Right ast

      it "cross-language roundtrip for record pattern" $
        let ast = RecordPat (Name "Foo") [(Name "bar", VarPat (Name "x"))]
            hsText = runPrint haskellPat ast
        in case runParse purescriptPat hsText of
             Left err -> expectationFailure (show err)
             Right psPat ->
               let psText = runPrint purescriptPat psPat
               in runParse haskellPat psText `shouldBe` Right ast

      it "RecordPat in case alt" $
        runParse haskellExpr "case x of { Foo { bar = y } -> y }" `shouldBe`
          Right (Case (Var (Name "x"))
            [CaseAlt (RecordPat (Name "Foo") [(Name "bar", VarPat (Name "y"))]) [] (Var (Name "y"))])

      it "QCon-based record construction roundtrips" $
        let ast = RecordUpdate (QCon [Name "Data", Name "Foo"] (Name "MkBar")) [(Name "x", Literal (IntLit 1))]
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "qualified type roundtrips: TyQCon" $
        let ast = Ann (Var (Name "x")) (TyQCon [Name "Data", Name "Map"] (Name "Map"))
        in roundtrip haskellExpr ast `shouldBe` Right ast

      it "QCon cross-language roundtrips" $
        let ast = QCon [Name "Data", Name "Map"] (Name "Map")
            hsText = runPrint haskellExpr ast
        in case runParse purescriptExpr hsText of
             Left err -> expectationFailure (show err)
             Right psExpr ->
               let psText = runPrint purescriptExpr psExpr
               in runParse haskellExpr psText `shouldBe` Right ast

      it "reserved word after dot is NOT qualified: Foo.case" $
        -- Foo.case should parse as Con "Foo", then fail or parse "case" as keyword
        -- It should NOT parse as QVar ["Foo"] "case"
        case runParse haskellExpr "Foo.case" of
          Right (QVar _ _) -> expectationFailure "should not parse as QVar"
          _ -> pure ()

      it "qualified in application context: Data.Map.insert k v" $
        runParse haskellExpr "Data.Map.insert k v" `shouldBe`
          Right (App (App (QVar [Name "Data", Name "Map"] (Name "insert")) (Var (Name "k"))) (Var (Name "v")))

      it "multiple qualified names in expression" $
        runParse haskellExpr "Data.Map.insert k v Data.Map.empty" `shouldBe`
          Right (App (App (App (QVar [Name "Data", Name "Map"] (Name "insert")) (Var (Name "k"))) (Var (Name "v")))
            (QVar [Name "Data", Name "Map"] (Name "empty")))
