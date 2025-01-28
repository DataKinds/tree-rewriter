{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE UndecidableInstances #-}

-- This module parses and matches patterns in the input tree for use in the Runtime.
-- All relevant datatypes for extracting information from the input tree are also compiled here.
module Recognizers where

import qualified Data.Text as T
import Core (Tree (..), RValue (..))
import Definitions (EatenDef (..), MatchCondition (..), MatchEffect (..), UseCount (..))


pattern LeafSym :: T.Text -> Tree RValue
pattern LeafSym sym = Leaf (RSymbol sym)

pattern LeafStr :: T.Text -> Tree RValue
pattern LeafStr sym = Leaf (RString sym)

data DefOpType = SetOp | TreeOp
acceptOp :: Tree RValue -> Maybe (DefOpType, UseCount)
acceptOp (Branch _) = Nothing
acceptOp (Leaf (RSymbol "~>")) = Just (TreeOp, UseMany)
acceptOp (Leaf (RSymbol "~")) = Just (TreeOp, UseOnce)
acceptOp (Leaf (RSymbol "|>")) = Just (SetOp, UseMany)
acceptOp (Leaf (RSymbol "|")) = Just (SetOp, UseOnce)
acceptOp (Leaf _) = Nothing

-- Given a tree, is the head of it listing out a rewrite rule?
recognizeDef :: Tree RValue -> Maybe EatenDef
recognizeDef (Leaf _) = Nothing
recognizeDef (Branch trees) = case trees of
    pat:(acceptOp -> Just (opType, useCount)):effect:(LeafSym "&"):pat':(acceptOp -> Just (opType', useCount')):effects' -> 
        let useCombo = if useCount == UseMany || useCount' == UseMany then UseMany else UseOnce
        in pure $ EatenDef useCombo [matchCond opType pat, matchCond opType' pat'] [matchEff opType [effect], matchEff opType effects']
    pat:(acceptOp -> Just (opType, useCount)):effects -> 
        pure $ EatenDef useCount [matchCond opType pat] [matchEff opType effects]
    _ -> Nothing
    where
        matchCond :: DefOpType -> Tree RValue -> MatchCondition
        matchCond TreeOp = TreePattern
        matchCond SetOp = MultisetPattern
        matchEff :: DefOpType -> [Tree RValue] -> MatchEffect
        matchEff TreeOp = TreeReplacement
        matchEff SetOp = MultisetPush


-- Built in rules parsed from the input tree!
data BuiltinRule = BuiltinRule {
    builtinName :: T.Text,
    builtinArgs :: [Tree RValue]
}

-- Given a tree, is the head of it listing out a builtin invocation?
recognizeBuiltin :: Tree RValue -> Maybe BuiltinRule
recognizeBuiltin (Leaf _) = Nothing
recognizeBuiltin (Branch trees) = case trees of
    (LeafSym "@"):[LeafSym "bag"] -> pure $ BuiltinRule "bag" []
    (LeafSym "@"):[LeafSym "version"] -> pure $ BuiltinRule "version" []
    (LeafSym "@"):(LeafSym "parse"):input@[LeafStr _] -> pure $ BuiltinRule "parse" input
    (LeafSym "@"):(LeafSym "cat"):path@[LeafStr _] -> pure $ BuiltinRule "cat" path
    _ -> Nothing