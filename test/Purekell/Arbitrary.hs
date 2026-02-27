{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Purekell.Arbitrary
  ( genIdent
  , genUpperIdent
  , noRecordAccess
  , noTuple
  , noTuplePat
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

import Purekell.AST
import Purekell.Instance (MethodEquation (..))

-- | Generate a valid Haskell/PureScript lowercase identifier
genIdent :: Gen Text
genIdent = do
  c <- elements ['a'..'z']
  cs <- listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])
  let ident = T.pack (c : cs)
  if ident `elem` keywords then genIdent else pure ident
  where
    keywords :: [Text]
    keywords = [ "case", "of", "let", "in", "where", "do", "if", "then"
               , "else", "data", "type", "class", "instance", "module"
               , "import", "forall", "infixl", "infixr", "infix" ]

-- | Generate a valid uppercase identifier (for constructors)
genUpperIdent :: Gen Text
genUpperIdent = do
  c <- elements ['A'..'Z']
  cs <- listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])
  pure (T.pack (c : cs))

-- | Generate a valid operator name
genOperator :: Gen Name
genOperator = Name <$> elements
  ["==", "/=", "<", ">", "<=", ">=", "<>", "&&", "||", "+", "*"]

instance Arbitrary Name where
  arbitrary = Name <$> genIdent

instance Arbitrary Lit where
  arbitrary = oneof
    [ IntLit . getNonNegative <$> arbitrary
    , FloatLit <$> (getNonNegative <$> arbitrary) `suchThat` (\d -> not (isNaN d) && not (isInfinite d))
    , StringLit <$> genSafeString
    , CharLit <$> elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
    ]
    where
      genSafeString = T.pack <$> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' '])

instance Arbitrary Guard where
  arbitrary = sized $ \n -> Guard <$> resize (n `div` 2) arbitrary

instance Arbitrary CaseAlt where
  arbitrary = sized $ \n -> do
    pat <- resize (n `div` 3) arbitrary
    numGuards <- choose (0, 1)
    guards <- vectorOf numGuards (resize (n `div` 3) arbitrary)
    body <- resize (n `div` 3) arbitrary
    pure (CaseAlt pat guards body)

  shrink (CaseAlt p gs e) =
    [CaseAlt p [] e | not (null gs)]
    ++ [CaseAlt p gs e' | e' <- shrink e]

instance Arbitrary Binding where
  arbitrary = sized $ \n -> Binding <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary

  shrink (Binding p e) = [Binding p e' | e' <- shrink e]

instance Arbitrary Stmt where
  arbitrary = sized $ \n -> oneof
    [ StmtBind <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
    , StmtExpr <$> resize (n `div` 2) arbitrary
    , do numBinds <- choose (1, 2)
         StmtLet <$> vectorOf numBinds (resize (n `div` 3) arbitrary)
    ]

  shrink (StmtBind _ e) = [StmtExpr e]
  shrink (StmtExpr e) = [StmtExpr e' | e' <- shrink e]
  shrink (StmtLet bs) = [StmtLet bs' | bs' <- shrinkList shrink bs, not (null bs')]

instance Arbitrary Expr where
  arbitrary = sized go
    where
      go 0 = oneof
        [ Literal <$> arbitrary
        , Var <$> arbitrary
        , Con . Name <$> genUpperIdent
        ]
      go n = frequency
        [ (3, go 0)
        , (2, App <$> go half <*> go half)
        , (2, InfixApp <$> go half <*> genOperator <*> go half)
        , (1, do numPats <- choose (1, 2)
                 pats <- vectorOf numPats (resize half arbitrary)
                 body <- go half
                 pure (Lam pats body))
        , (1, If <$> go third <*> go third <*> go third)
        , (1, do scrut <- go half
                 numAlts <- choose (1, 2)
                 alts <- vectorOf numAlts (resize half arbitrary)
                 pure (Case scrut alts))
        , (1, do numBinds <- choose (1, 2)
                 bindings <- vectorOf numBinds (resize half arbitrary)
                 body <- go half
                 pure (Let bindings body))
        , (1, do numStmts <- choose (1, 3)
                 stmts <- vectorOf numStmts (resize half arbitrary)
                 pure (Do stmts))
        , (1, Neg <$> go half)
        , (1, RecordAccess <$> go half <*> (Name <$> genIdent))
        , (1, do numElems <- choose (2, 3)
                 Tuple <$> vectorOf numElems (go (n `div` (numElems + 1))))

        ]
        where
          half = n `div` 2
          third = n `div` 3

  shrink (App f x) = [f, x] ++ [App f' x | f' <- shrink f] ++ [App f x' | x' <- shrink x]
  shrink (InfixApp l _ r) = [l, r]
  shrink (Lam _ body) = [body]
  shrink (If c t e) = [c, t, e]
  shrink (Case scrut alts) = scrut : [body | CaseAlt _ _ body <- alts]
  shrink (Let _ body) = [body]
  shrink (Do stmts) = [e | StmtExpr e <- stmts]
  shrink (Neg e) = e : [Neg e' | e' <- shrink e]
  shrink (RecordAccess rec _) = [rec]
  shrink (Tuple es) = es
  shrink _ = []

instance Arbitrary Pat where
  arbitrary = sized go
    where
      go 0 = oneof
        [ VarPat <$> arbitrary
        , LitPat <$> arbitrary
        , pure WildPat
        , ConPat . Name <$> genUpperIdent <*> pure []
        ]
      go n = oneof
        [ go 0
        , do con <- Name <$> genUpperIdent
             numArgs <- choose (1, 3)
             args <- vectorOf numArgs (go (n `div` (numArgs + 1)))
             pure (ConPat con args)
        , do numElems <- choose (2, 3)
             TuplePat <$> vectorOf numElems (go (n `div` (numElems + 1)))
        ]

  shrink (ConPat n args) = [ConPat n (take i args) | i <- [0 .. length args - 1]]
                        ++ [ConPat n args' | args' <- shrinkList shrink args]
  shrink (TuplePat ps) = ps ++ [TuplePat ps' | ps' <- shrinkList shrink ps, length ps' >= 2]
  shrink _ = []

-- | Check recursively that an expression tree contains no RecordAccess.
-- RecordAccess only roundtrips in PureScript (dot syntax), not Haskell
-- (prints as function application) or Instance (no dot access parser).
noRecordAccess :: Expr -> Bool
noRecordAccess (Neg e) = noRecordAccess e
noRecordAccess (RecordAccess _ _) = False
noRecordAccess (App f x) = noRecordAccess f && noRecordAccess x
noRecordAccess (InfixApp l _ r) = noRecordAccess l && noRecordAccess r
noRecordAccess (Lam _ e) = noRecordAccess e
noRecordAccess (If c t e) = noRecordAccess c && noRecordAccess t && noRecordAccess e
noRecordAccess (Case scrut alts) = noRecordAccess scrut && all altOk alts
  where altOk (CaseAlt _ gs e) = all guardOk gs && noRecordAccess e
        guardOk (Guard e) = noRecordAccess e
noRecordAccess (Let bs e) = all bindOk bs && noRecordAccess e
  where bindOk (Binding _ e') = noRecordAccess e'
noRecordAccess (Do stmts) = all stmtOk stmts
  where stmtOk (StmtBind _ e) = noRecordAccess e
        stmtOk (StmtExpr e) = noRecordAccess e
        stmtOk (StmtLet bs) = all (\(Binding _ e) -> noRecordAccess e) bs
noRecordAccess (Tuple es) = all noRecordAccess es
noRecordAccess _ = True

-- | Check recursively that an expression tree contains no Tuple.
-- Tuple doesn't roundtrip in PureScript (prints as App (Con "Tuple") ...).
noTuple :: Expr -> Bool
noTuple (Tuple _) = False
noTuple (Neg e) = noTuple e
noTuple (RecordAccess e _) = noTuple e
noTuple (App f x) = noTuple f && noTuple x
noTuple (InfixApp l _ r) = noTuple l && noTuple r
noTuple (Lam ps e) = all noTuplePat ps && noTuple e
noTuple (If c t e) = noTuple c && noTuple t && noTuple e
noTuple (Case scrut alts) = noTuple scrut && all altOk alts
  where altOk (CaseAlt p gs e) = noTuplePat p && all guardOk gs && noTuple e
        guardOk (Guard e) = noTuple e
noTuple (Let bs e) = all bindOk bs && noTuple e
  where bindOk (Binding p e') = noTuplePat p && noTuple e'
noTuple (Do stmts) = all stmtOk stmts
  where stmtOk (StmtBind p e) = noTuplePat p && noTuple e
        stmtOk (StmtExpr e) = noTuple e
        stmtOk (StmtLet bs) = all (\(Binding p e) -> noTuplePat p && noTuple e) bs
noTuple _ = True

-- | Check that a pattern contains no TuplePat (and no Tuple in nested expressions).
noTuplePat :: Pat -> Bool
noTuplePat (TuplePat _) = False
noTuplePat (ConPat _ args) = all noTuplePat args
noTuplePat _ = True

instance Arbitrary MethodEquation where
  arbitrary = sized $ \n -> do
    name <- Name <$> genIdent
    numPats <- choose (0, 3)
    pats <- vectorOf numPats (resize (n `div` 3) arbitrary)
    numGuards <- choose (0, 1)
    guards <- vectorOf numGuards (resize (n `div` 3) arbitrary)
    body <- resize (n `div` 2) arbitrary
    pure (MethodEquation name pats guards body)

  shrink (MethodEquation name pats guards body) =
    [MethodEquation name pats [] body | not (null guards)]
    ++ [MethodEquation name [] guards body | not (null pats)]
    ++ [MethodEquation name pats guards body' | body' <- shrink body]
