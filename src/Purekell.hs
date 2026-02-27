{-# LANGUAGE OverloadedStrings #-}

module Purekell
  ( hsToPs
  , psToHs
  , module Purekell.AST
  , module Purekell.Codec
  , module Purekell.Instance
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle)

import Purekell.AST
import Purekell.Codec
import Purekell.Haskell (haskellExpr)
import Purekell.Instance
import Purekell.PureScript (purescriptExpr)

-- | Translate a Haskell expression to PureScript
hsToPs :: Text -> Either (ParseErrorBundle Text Void) Text
hsToPs input = do
  ast <- runParse haskellExpr input
  pure (runPrint purescriptExpr ast)

-- | Translate a PureScript expression to Haskell
psToHs :: Text -> Either (ParseErrorBundle Text Void) Text
psToHs input = do
  ast <- runParse purescriptExpr input
  pure (runPrint haskellExpr ast)
