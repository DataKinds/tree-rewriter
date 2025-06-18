
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


-- | This module implements the datatypes and functions required to match and apply Rosin rules.
-- This module does not have the best name. It holds all the Runtime related types, and all the abstractions
-- over matching patterns and applying effects based off the bindings those patterns produced.
-- TODO: Rename to RuntimeRules? RuntimeBindings?
module RuntimeEffects where
import Core 
import qualified Zipper as Z
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (runStateT), State, runState, state, get, put)
import Data.Maybe (isJust)
import qualified Multiset as MS
import Data.Semigroup (Semigroup(sconcat), Any (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Functor.Identity (Identity(..))
import Optics.State
import Optics


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
data MatchEffect = TreeReplacement [Tree RValue] | MultisetPush (MS.Multiset (Tree RValue)) deriving (Eq, Show)

-- | Rosin rule definition type
data MatchRule = MatchRule {
    useCount :: UseCount,
    matchCondition :: [MatchCondition],
    matchEffect :: [MatchEffect]
} deriving (Eq, Show)

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
    -- Where are we in the data tree?
    runtimeZipper :: Z.Zipper RValue,
    -- Multiset state!
    runtimeMultiset :: MS.Multiset (Tree RValue),
    -- Epoch number: incremented every time we apply a rule or change our state
    runtimeEpoch :: Int,
    -- Done marker: are we ready to finish execution?
    runtimeAreWeDoneYet :: Bool
} deriving (Show)
makeFieldLabels ''Runtime
type RuntimeM m = StateT Runtime m


instance Semigroup MatchRule where
    (MatchRule uc mcs mes) <> (MatchRule uc' mcs' mes') = MatchRule (uc <> uc') (mcs <> mcs') (mes <> mes')

instance Monoid MatchRule where
    mempty = MatchRule mempty [] []


hoistState :: (Monad m) => State s a -> StateT s m a
hoistState = state . runState

-- | Apply matching conditions from a definition according to a given runtime.
-- False if we fail to apply a condition. True if they all apply.
tryBindConditions :: [MatchCondition] -> Runtime -> Binder_ Bool -- False if a condition didn't apply
tryBindConditions [] _ = pure True
tryBindConditions (cond:xs) r = do
    success <- applyMatchCondition cond r
    go <- tryBindConditions xs r
    pure $ success && go

-- | Applies matching rules from a list of rules, without applying the effects.
-- Gives back the first rule where every condition matched, or nothing.
tryDefinitions :: [MatchRule] -> Runtime -> Binder_ (Maybe MatchRule)
tryDefinitions [] _ = pure Nothing
tryDefinitions (rule:rules) r = let
    conditions = matchCondition rule
    in do
        success <- put emptyBinder >> tryBindConditions conditions r
        if success 
            then pure $ Just rule
            else tryDefinitions rules r


-- Apply matching conditions from a list of definitions against another tree that isn't the runtime zipper's focus
-- Gives back the first definition where every condition matched, if it exists.
tryDefinitionsAt :: [MatchRule] -> Runtime -> Tree RValue -> Binder_ (Maybe MatchRule)
tryDefinitionsAt defs r subject = let
    r' = over #zipper (`Z.put` subject) r
    in tryDefinitions defs r'


-- | Apply a MatchEffect to a given runtime, monadically
-- Sequence with a successful `applyMatchCondition` to mutate the runtime state based on a definition.
applyMatchEffect :: MatchEffect -> RuntimeM Binder_ ()
applyMatchEffect (MultisetPush ms) = do
    ms' <- lift $ MS.traverseValues (fmap rebranch . betaReduce) ms
    modifying #multiset (MS.putMany ms')
applyMatchEffect (TreeReplacement []) = modifying #zipper Z.dropFocus
applyMatchEffect (TreeReplacement template) = do
    binder <- lift get
    let (rewritten, _) = runIdentity $ mapM betaReduce template `runStateT` binder
    modifying #zipper (`Z.spliceIn` concat rewritten)


treeMapReduce :: Semigroup a => (Tree b -> a) -> Tree b -> a
treeMapReduce mapper input@(Leaf _) = mapper input
treeMapReduce mapper input@(Branch xs) = sconcat $ mapper input :| (treeMapReduce mapper <$> xs)


-- | Bind variables associated with a match condition, or fail out and return False
applyMatchCondition :: MatchCondition -> Runtime -> Binder_ Bool
applyMatchCondition (MultisetPattern ms) r = let
    pocket = runtimeMultiset r
    in do
        ms' <- MS.traverseValues (fmap rebranch . betaReduce) ms
        pure $ MS.allInside ms' pocket -- TODO: pattern match!
applyMatchCondition (TreePattern pat) r = get >>= \binding -> let -- TODO: beta reduce, in case this condition comes after the multiset
    rules = runtimeRules r
    subject = Z.look . runtimeZipper $ r
    -- construct the eager matcher
    eagerMatcherCallStep = Any . isJust . fst . flip runState binding <$> tryDefinitionsAt rules r
    -- eagerMatcherDefStep = Any . isJust . recognizeDef 
    -- eagerMatcherBuiltinStep = Any . isJust . recognizeBuiltin 
    eagerMatcherStep i = eagerMatcherCallStep i -- <> eagerMatcherDefStep i <> eagerMatcherBuiltinStep i
    eagerMatcher = treeMapReduce eagerMatcherStep
    in hoistState $ tryApply (getAny . eagerMatcher) subject pat


