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
import Core (Tree (..), RValue (..), rebranch)
import RuntimeEffects (MatchRule (..), MatchCondition (..), MatchEffect (..), UseCount (..))
import qualified Multiset as MS
import Data.Maybe (isJust, listToMaybe, catMaybes)
import Control.Monad.Trans.Writer.CPS (Writer, tell, execWriter)


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
pocketCopies nTimes = MS.fromList . map (,nTimes)

-- Read a definition from a stream of tree tokens
eatCondEffectPair :: [Tree RValue] -> Writer MatchRule [Tree RValue]
eatCondEffectPair [] = pure []
eatCondEffectPair [_] = pure []
eatCondEffectPair candidate = let 
    (condOpEff, andRest) = break (== Leaf (RSymbol "&")) candidate 
    (cond, opEff) = break (isJust . acceptOp) condOpEff
    eff = dropWhile (isJust . acceptOp) opEff
    rest = dropWhile (== Leaf (RSymbol "&")) andRest
    in case listToMaybe opEff >>= acceptOp of  
        Just (TreeOp, nUse) -> do
            tell $ MatchRule nUse [TreePattern $ rebranch cond] [TreeReplacement eff]
            pure rest
        Just (SetOp, nUse) -> do
            let pat = if null cond then [] else [MultisetPattern . pocketCopies 1 $ cond]
            let pushes = catMaybes [if null eff then Nothing else Just $ pocketCopies 1 eff, if null cond then Nothing else Just $ pocketCopies (-1) cond]
            tell $ MatchRule nUse pat (MultisetPush <$> pushes)
            pure rest
        Nothing -> pure []

-- Rerun an action from an initial state until it produces an empty list
deplete :: (Monad m) => ([a] -> m [a]) -> [a] -> m ()
deplete f x = f x >>= go
    where 
        go [] = pure ()
        go st = deplete f st

-- Given a tree, is the head of it listing out a rewrite rule?
recognizeDef :: Tree RValue -> Maybe MatchRule
recognizeDef (Leaf _) = Nothing
recognizeDef (Branch trees) = let
    ingestedDef = execWriter $ deplete eatCondEffectPair trees
    in if ingestedDef == mempty then Nothing else Just ingestedDef

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
    (LeafSym "@"):[LeafSym "getLine"] -> pure $ BuiltinRule "getLine" []
    (LeafSym "@"):(LeafSym "parse"):input@[LeafStr _] -> pure $ BuiltinRule "parse" input
    (LeafSym "@"):(LeafSym "cat"):path@[LeafStr _] -> pure $ BuiltinRule "cat" path
    (LeafSym "@"):(LeafSym "print"):out -> pure $ BuiltinRule "print" out
    _ -> Nothing