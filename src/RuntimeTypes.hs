{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}

module RuntimeTypes where

import Core
import qualified Zipper as Z
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Trans.Class ( lift, lift )
import Control.Monad.Trans.State
import Control.Monad ( when, ap, void )
import Data.Maybe ( isJust, fromJust, isJust, fromMaybe )
import qualified Multiset as MS
import System.FilePath ((</>), takeDirectory)
import Parser (parse)
import Data.Bool (bool)
import Data.List (intercalate)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Multiset (cleanUp)
import Optics.State
import Data.Functor (void)
import Data.Foldable (for_)
import Control.Arrow ((&&&))
import Prettyprinter.Render.Terminal (putDoc, color, Color (Red))
import Data.Semigroup (Semigroup(sconcat), Any (..))
import Data.Functor.Identity (Identity(..))
import Optics
import Debug.Trace (trace)
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
-- type RuntimeM m = StateT Runtime m

class (MonadState r m, HasRuntime r) => MonadRuntime r (m :: Type -> Type) where
instance (MonadState r m, HasRuntime r) => MonadRuntime r m where

-- hoistState :: (Monad m) => State s a -> StateT s m a
-- hoistState = state . runState

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
