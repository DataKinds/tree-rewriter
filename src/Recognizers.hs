{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

-- This module parses and matches patterns in the input tree for use in the Runtime.
-- Rule definitions and builtin rules are currently detected and parsed through this module.
-- All relevant datatypes for extracting information from the input tree are also compiled here.
module Recognizers where

import qualified Multiset as MS
import qualified Data.Text as T
import Core (Tree (..), RValue (..), unbranch, sexprprint)
import Data.Function (on)

-- Recognizer datatypes! --

-- What action does matching this rewrite rule require on the multiset? 
data MultisetAction = MultisetAction {
    multisetPops :: MS.Multiset (Tree RValue),
    multisetPushs :: MS.Multiset (Tree RValue)
} deriving (Eq)
instance Show MultisetAction where
    show (MultisetAction pops pushs) = let 
        strPops = if MS.null pops then "" else show pops
        strPushs = if MS.null pushs then "" else show pushs
        in concat [strPops, "|", strPushs]

-- is this definition single use or will it apply forever?
data UseCount = UseOnce | UseMany deriving (Show, Eq)

-- What types of definition are there?
data EatenDef = TreeDef UseCount (Tree RValue) [Tree RValue]
              | MultisetDef UseCount MultisetAction
              | ComboDef UseCount MultisetAction (Tree RValue) [Tree RValue] deriving (Eq)

isMultisetDef :: EatenDef -> Bool
isMultisetDef (MultisetDef _ _) = True
isMultisetDef _ = False

instance Show EatenDef where
    show (TreeDef useCount pat templates) = let 
        op = if useCount == UseOnce then " ~ " else " ~> "
        in concat [sexprprint pat, op, unwords $ sexprprint <$> templates]
    show (MultisetDef useCount multisetAction) = show multisetAction -- TODO: shows UseMany rules wrong
    show (ComboDef useCount multisetAction pat templates) = let 
        op = if useCount == UseOnce then " ~ " else " ~> "
        in concat [show multisetAction, " & ", sexprprint pat, op, unwords $ sexprprint <$> templates]

-- Built in rules parsed from the input tree!
data BuiltinRule = BuiltinRule {
    builtinName :: T.Text,
    builtinArgs :: [Tree RValue]
}

-- Done with datatypes! --

pattern LeafSym :: T.Text -> Tree RValue
pattern LeafSym sym = Leaf (RSymbol sym)

pattern LeafStr :: T.Text -> Tree RValue
pattern LeafStr sym = Leaf (RString sym)

-- Given a tree, is the head of it listing out a rewrite rule?
recognizeDef :: Tree RValue -> Maybe EatenDef
recognizeDef (Leaf _) = Nothing
recognizeDef (Branch trees) = case trees of
    pat:(LeafSym "~>"):templates -> pure $ TreeDef UseMany pat templates
    pat:(LeafSym "~"):templates -> pure $ TreeDef UseOnce pat templates
    (pops:(LeafSym "|"):pushs:(LeafSym "&"):pat:(LeafSym "~>"):templates) -> 
        pure $ ComboDef UseMany (maFromTrees pops pushs) pat templates
    (pops:(LeafSym "|"):pushs:(LeafSym "&"):pat:(LeafSym ">"):templates) -> 
        pure $ ComboDef UseOnce (maFromTrees pops pushs) pat templates
    (pops:(LeafSym "|>"):[pushs]) -> pure . MultisetDef UseMany $ maFromTrees pops pushs
    (pops:(LeafSym "|"):[pushs]) -> pure . MultisetDef UseOnce $ maFromTrees pops pushs
    -- TODO: figure out a better way to do all these variations LOL
    ((LeafSym "|"):pushs:(LeafSym "&"):pat:(LeafSym "~>"):templates) -> 
        pure $ ComboDef UseMany (maFromTrees (Branch []) pushs) pat templates
    ((LeafSym "|"):pushs:(LeafSym "&"):pat:(LeafSym ">"):templates) -> 
        pure $ ComboDef UseOnce (maFromTrees (Branch []) pushs) pat templates
    ((LeafSym "|>"):[pushs]) -> pure . MultisetDef UseMany $ maFromTrees (Branch []) pushs
    ((LeafSym "|"):[pushs]) -> pure . MultisetDef UseOnce $ maFromTrees (Branch []) pushs

    (pops:(LeafSym "|"):(LeafSym "&"):pat:(LeafSym "~>"):templates) -> 
        pure $ ComboDef UseMany (maFromTrees pops (Branch [])) pat templates
    (pops:(LeafSym "|"):(LeafSym "&"):pat:(LeafSym ">"):templates) -> 
        pure $ ComboDef UseOnce (maFromTrees pops (Branch [])) pat templates
    (pops:[LeafSym "|>"]) -> pure . MultisetDef UseMany $ maFromTrees pops (Branch [])
    (pops:[LeafSym "|"]) -> pure . MultisetDef UseOnce $ maFromTrees pops (Branch [])

    _ -> Nothing
    where
        multisetFromTree :: Tree RValue -> MS.Multiset (Tree RValue)
        multisetFromTree = MS.fromList . map (,1) . unbranch
        maFromTrees :: Tree RValue -> Tree RValue -> MultisetAction
        maFromTrees = MultisetAction `on` multisetFromTree

-- Given a tree, is the head of it listing out a builtin invocation?
recognizeBuiltin :: Tree RValue -> Maybe BuiltinRule
recognizeBuiltin (Leaf _) = Nothing
recognizeBuiltin (Branch trees) = case trees of
    (LeafSym "@"):[LeafSym "bag"] -> pure $ BuiltinRule "bag" []
    (LeafSym "@"):[LeafSym "version"] -> pure $ BuiltinRule "version" []
    (LeafSym "@"):(LeafSym "parse"):input@[LeafStr _] -> pure $ BuiltinRule "parse" input
    (LeafSym "@"):(LeafSym "cat"):path@[LeafStr _] -> pure $ BuiltinRule "cat" path
    _ -> Nothing