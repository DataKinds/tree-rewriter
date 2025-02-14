{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE UndecidableInstances #-}

-- This module parses and matches patterns in the input tree for use in the Runtime.
-- All relevant datatypes for extracting information from the input tree are also compiled here.
module Recognizers where

import qualified Data.Text as T
import Core (Tree (..), RValue (..), unbranch, rebranch)
import Definitions (EatenDef (..), MatchCondition (..), MatchEffect (..), UseCount (..))
import qualified Multiset as MS
import Data.Maybe (isJust, listToMaybe)


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

pocketCopies :: Int -> [Tree RValue] -> MS.Multiset (Tree RValue)
pocketCopies nTimes = MS.fromList . map (,nTimes) . concatMap unbranch

-- Read a definition from a stream of tree tokens
eatCondEffectPair :: [Tree RValue] -> Maybe (EatenDef, [Tree RValue])
eatCondEffectPair [] = Nothing
eatCondEffectPair [_] = Nothing
eatCondEffectPair candidate = let 
    (condOpEff, andRest) = break (== Leaf (RSymbol "&")) candidate 
    (cond, opEff) = break (isJust . acceptOp) condOpEff
    eff = dropWhile (isJust . acceptOp) opEff
    rest = dropWhile (== Leaf (RSymbol "&")) andRest
    in listToMaybe opEff >>= acceptOp >>= \case 
        (TreeOp, nUse) -> 
            pure (EatenDef nUse [TreePattern $ rebranch cond] [TreeReplacement eff], rest)
        (SetOp, nUse) -> 
            pure (EatenDef nUse [MultisetPattern . pocketCopies 1 $ cond] [MultisetPush t | t <- [pocketCopies 1 eff, pocketCopies (-1) cond]], rest)

-- Given a tree, is the head of it listing out a rewrite rule?
recognizeDef :: Tree RValue -> Maybe EatenDef
recognizeDef (Leaf _) = Nothing
recognizeDef (Branch trees) = case trees of
    -- TODO: support no pat, support no effect, all combinations here
    pat:(acceptOp -> Just (opType, useCount)):effect:(LeafSym "&"):pat':(acceptOp -> Just (opType', useCount')):effects' -> 
        let useCombo = if useCount == UseMany || useCount' == UseMany then UseMany else UseOnce
        in pure $ EatenDef useCombo [matchCond opType pat, matchCond opType' pat'] $ matchEff opType (Just pat) [effect] ++ matchEff opType (Just pat') effects'
    pat:(acceptOp -> Just (opType, useCount)):effects -> 
        pure $ EatenDef useCount [matchCond opType pat] (matchEff opType (Just pat) effects)
    (acceptOp -> Just (opType, useCount)):effects -> 
        pure $ EatenDef useCount [] (matchEff opType Nothing effects)
    pat:[acceptOp -> Just (opType, useCount)] -> 
        pure $ EatenDef useCount [matchCond opType pat] []
    _ -> Nothing
    where
        pushTheseTerms :: Int -> [Tree RValue] -> MS.Multiset (Tree RValue)
        pushTheseTerms nTimes = MS.fromList . map (,nTimes) . concatMap unbranch
        matchCond :: DefOpType -> Tree RValue -> MatchCondition
        matchCond TreeOp = TreePattern
        matchCond SetOp = MultisetPattern . pushTheseTerms 1 . pure
        matchEff :: DefOpType -> Maybe (Tree RValue) -> [Tree RValue] -> [MatchEffect]
        matchEff TreeOp _ = pure . TreeReplacement
        matchEff SetOp Nothing = pure . MultisetPush . pushTheseTerms 1
        matchEff SetOp (Just matchedPat) = \effs -> MultisetPush <$> [pushTheseTerms 1 effs, pushTheseTerms (-1) [matchedPat]]


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