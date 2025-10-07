{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}

module RuntimeTypes where

import Core
import qualified Zipper as Z
import qualified Multiset as MS
import Data.List (intercalate)
import Optics
import Data.Kind (Type)
import Control.Monad.State.Class


-- import Prettyprinter

-- is this definition single use or will it apply forever?
data UseCount = UseOnce | UseMany deriving (Show, Eq)

instance Semigroup UseCount where
    UseMany <> _ = UseMany
    UseOnce <> b = b

instance Monoid UseCount where
    mempty = UseOnce -- identity of <>


-- | What's required for this definition to match?
data MatchCondition = TreePattern (Tree RValue) | MultisetPattern (MS.Multiset (Tree RValue)) deriving (Eq, Show)

-- | What happens when a definition matches?
data MatchEffect
    = TreeReplacement [Tree RValue] -- Replace the focus of the runtime with a different tree
    | MultisetPush (MS.Multiset (Tree RValue)) -- Push some terms into the multiset
    | Force PVar -- Completely evaluate the given binding given the current runtime
    deriving (Eq, Show)

-- | Rosin rule definition type
data MatchRule = MatchRule {
    useCount :: UseCount,
    matchCondition :: [MatchCondition],
    matchEffect :: [MatchEffect]
} deriving (Eq, Show)

prettyMatchRule :: MatchRule -> String
prettyMatchRule (MatchRule UseOnce conditions effects) = unwords $ map show conditions ++ ["~"] ++ map show effects
prettyMatchRule (MatchRule UseMany conditions effects) = unwords $ map show conditions ++ ["~>"] ++ map show effects

-- | Runtime handles the state of the rewrite head processing the input data 
data Runtime = Runtime {
    -- What file are we executing
    runtimePath :: String,
    -- Do we print out debug information?
    runtimeVerbose :: Bool,
    -- What rewriting rules are active?
    runtimeRules :: [MatchRule],
    -- What rewriting lambdas are active?
    runtimeSingleUseRules :: [MatchRule],
    -- Where are we in the data tree? Note that we're also using a tag on every level 
    runtimeZipper :: Z.Zipper RValue,
    -- Multiset state!
    runtimeMultiset :: MS.Multiset (Tree RValue),
    -- Epoch number: incremented every time we apply a rule or change our state
    runtimeEpoch :: Int,
    -- Empty cycle: true if we didn't apply any rules this iteration over the tree
    runtimeEmptyCycle :: Bool,
    -- Empty cycle count: how many times have we looped over the tree without matching anything?
    runtimeEmptyCycleCount :: Int
} deriving (Show)
makeFieldLabels ''Runtime
makeClassy ''Runtime

instance HasRuntime r => HasRuntime (r, x) where
    runtime = _1 % runtime

class (MonadBinder r m, MonadState r m, HasRuntime r) => MonadRuntime r (m :: Type -> Type) where
instance (MonadBinder r m, MonadState r m, HasRuntime r) => MonadRuntime r m where

-- | Construct an empty runtime, ready to process a tree
emptyRuntime :: String -> Bool -> [Tree RValue] -> Runtime
emptyRuntime filepath verbose' trees = Runtime filepath verbose' emptyRules emptyRules (Z.zipperFromTrees (epoch-1) trees) MS.empty epoch True 0
    where emptyRules = []
          epoch = 0

prettyRuntime :: Runtime -> String
prettyRuntime r = unlines [
    "Runtime: "
    , "    path: " ++ show (runtimePath r)
    , "    verbose: " ++ show (runtimeVerbose r)
    , "    rules: "
    , intercalate "\n" (("      * " ++) . prettyMatchRule <$> runtimeRules r)
    , "    singleUseRules: "
    , intercalate "\n" (("      * " ++) . prettyMatchRule <$> runtimeSingleUseRules r)
    , "    zipper: ... waiting for something to happen?"
    , "    multiset: " ++ show (runtimeMultiset r)
    , "    epoch: " ++ show (runtimeEpoch r)
    , "    emptyCycle: " ++ show (runtimeEmptyCycle r)
    , "    emptyCycleCount: " ++ show (runtimeEmptyCycleCount r)
    ]

instance Semigroup MatchRule where
    (MatchRule uc mcs mes) <> (MatchRule uc' mcs' mes') = MatchRule (uc <> uc') (mcs <> mcs') (mes <> mes')

instance Monoid MatchRule where
    mempty = MatchRule mempty [] []
