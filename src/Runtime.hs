{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Runtime where

import Core
    ( betaReduce,
      branch,
      emptyBinder,
      num,
      rebranch,
      str,
      sym,
      tryApply,
      Binder_,
      RValue(RString),
      Tree(..), tstr )
import qualified Zipper as Z
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (runStateT), modify, gets, execStateT, State, runState, state, get, put)
import Control.Monad (unless, when)
import Data.Maybe (isJust, fromJust)
import qualified Multiset as MS
import Recognizers (recognizeDef, recognizeBuiltin, BuiltinRule (..))
import Data.Bifunctor (Bifunctor(second))
import System.FilePath ((</>), takeDirectory)
import Parser (parse)
import Data.Bool (bool)
import Definitions (MatchCondition (..), EatenDef, UseCount (..), MatchEffect (..), defUseCount, defMatchCondition, defMatchEffect)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (find)
import Multiset (cleanUp)
import Data.Semigroup (Semigroup(sconcat), Any (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Functor.Identity (Identity(..))
import Optics.State
import Optics


-- Runtime handles the state of the rewrite head processing the input data 
data Runtime = Runtime {
    -- What file are we executing
    path :: String,
    -- Do we print out debug information?
    verbose :: Bool,
    -- What rewriting rules are active?
    rules :: [EatenDef],
    -- What rewriting lambdas are active?
    singleUseRules :: [EatenDef],
    -- Where are we in the data tree?
    zipper :: Z.Zipper RValue,
    -- Multiset state!
    multiset :: MS.Multiset (Tree RValue)
} deriving (Show)
makeFieldLabelsNoPrefix ''Runtime
type RuntimeTV m = StateT Runtime m
type RuntimeT m = RuntimeTV m ()
type RuntimeV = State Runtime

-- Add a new tree rewriting rule into the runtime
addRule :: Monad m => EatenDef -> RuntimeT m
addRule rule = modifying #rules (rule:)

-- Add a new single use tree rewriting rule into the runtime
addSingleUseRule :: Monad m => EatenDef -> RuntimeT m
addSingleUseRule rule = modifying #singleUseRules (rule:)

-- Execute a Rosin runtime --

-- Check the current position of the rewrite head. If it's pointing to a definition, consume it.
eatDef :: Monad m => RuntimeT m
eatDef = do
    subject <- gets (Z.look . Runtime.zipper)
    case recognizeDef subject of
        -- Add the rule definition to the runtime and snip it out from the input tree
        Just td -> case defUseCount td of
            UseOnce -> do
                addSingleUseRule td
                modifying #zipper Z.dropFocus
            UseMany -> do
                addRule td
                modifying #zipper (`Z.put` branch [sym "defined", str $ show td])
        Nothing -> pure ()

-- Execute a given builtin. The `args` passed in are the args passed to the builtin.
dispatchBuiltin :: [Tree RValue] -> T.Text -> RuntimeT IO
dispatchBuiltin args = \case
    "version" -> modifying #zipper (`Z.put` str "v0.0.0. That's right, We Aren't Semver Yet!")
    "bag" -> do
        bag <- gets (branch . map (\(x, n) -> branch [x, num n]) . MS.toList . Runtime.multiset)
        modifying #zipper (`Z.put` bag)
    "getLine" -> do
        line <- liftIO TIO.getLine
        modifying #zipper (`Z.put` tstr line)
    "print" -> do
        let printer = \case
                Leaf (RString input) -> TIO.putStrLn input
                other -> print other
        liftIO $ mapM_ printer args
        modifying #zipper Z.dropFocus
    "parse" -> case args of
        Leaf (RString input):_ -> do
            filepath <- gets Runtime.path
            case parse (T.unpack input) (filepath++"<eval>") of
                Left err -> modifying #zipper (`Z.put` (Leaf . RString . T.pack $ "parse error: " ++ show err))
                Right success -> modifying #zipper (`Z.spliceRight` success)
        _ -> pure ()
    "cat" -> case args of
        Leaf (RString path):_ -> do
            pathContext <- gets (takeDirectory . Runtime.path)
            fileContents <- lift . TIO.readFile . (pathContext </>) . T.unpack $ path
            modifying #zipper (`Z.put` (Leaf . RString $ fileContents))
        _ -> pure ()
    shouldntBePossible -> error$"Unrecognized builtin "++T.unpack shouldntBePossible++" that matched -- please report this as a bug!"

-- Check the current position of the rewrite head. If it's pointing to a builtin, rewrite it and execute any effects.
eatBuiltin :: RuntimeT IO
eatBuiltin = do
    subject <- gets (Z.look . Runtime.zipper)
    case recognizeBuiltin subject of
        Just (BuiltinRule name args) -> dispatchBuiltin args name
        Nothing -> pure ()

thrd :: (a, b, c) -> c
thrd (_,_,c) = c

hoistState :: (Monad m) => State s a -> StateT s m a
hoistState = state . runState

-- Apply matching conditions from a definition according to a given runtime.
-- False if we fail to apply a condition. True if they all apply.
tryBindConditions :: [MatchCondition] -> Runtime -> Binder_ Bool -- False if a condition didn't apply
tryBindConditions [] _ = pure True
tryBindConditions (cond:xs) r = do
    success <- applyMatchCondition cond r
    go <- tryBindConditions xs r
    pure $ success && go

-- Apply matching conditions from a list of definitions.
-- Gives back the first definition where every condition matched, if it exists.
tryDefinitions :: [EatenDef] -> Runtime -> Binder_ (Maybe EatenDef)
tryDefinitions defs r = let
    bindActions = [(d, (`tryBindConditions` r) . defMatchCondition $ d) | d <- defs]
    binded = second (`runState` emptyBinder) <$> bindActions
    success = find (fst . snd) binded
    in do
        maybe (pure ()) (put . snd . snd) success
        pure (fst <$> success)

-- Apply matching conditions from a list of definitions against another tree that isn't the runtime zipper's focus
-- Gives back the first definition where every condition matched, if it exists.
tryDefinitionsAt :: [EatenDef] -> Runtime -> Tree RValue -> Binder_ (Maybe EatenDef)
tryDefinitionsAt defs r subject = let
    r' = over #zipper (`Z.put` subject) r
    in tryDefinitions defs r'


treeMapReduce :: Semigroup a => (Tree b -> a) -> Tree b -> a
treeMapReduce mapper input@(Leaf _) = mapper input
treeMapReduce mapper input@(Branch xs) = sconcat $ mapper input :| (treeMapReduce mapper <$> xs)

-- Bind variables associated with a match condition, or fail out and return False
applyMatchCondition :: MatchCondition -> Runtime -> Binder_ Bool
applyMatchCondition (MultisetPattern ms) r = let
    pocket = Runtime.multiset r
    in do
        ms' <- MS.traverseValues (fmap rebranch . betaReduce) ms
        pure $ MS.allInside ms' pocket -- TODO: pattern match!
applyMatchCondition (TreePattern pat) r = get >>= \binding -> let -- TODO: beta reduce, in case this condition comes after the multiset
    rules = Runtime.rules r
    subject = Z.look . Runtime.zipper $ r
    -- construct the eager matcher
    eagerMatcherCallStep = Any . isJust . fst . flip runState binding <$> tryDefinitionsAt rules r
    eagerMatcherDefStep = Any . isJust . recognizeDef 
    eagerMatcherBuiltinStep = Any . isJust . recognizeBuiltin 
    eagerMatcherStep i = eagerMatcherCallStep i <> eagerMatcherDefStep i <> eagerMatcherBuiltinStep i
    eagerMatcher = treeMapReduce eagerMatcherStep
    in hoistState $ tryApply (getAny . eagerMatcher) subject pat

-- Sequence with a successful `applyMatchCondition` to mutate the runtime state based on a definition.
applyMatchEffect :: MatchEffect -> RuntimeTV Binder_ ()
applyMatchEffect (MultisetPush ms) = do
    ms' <- lift $ MS.traverseValues (fmap rebranch . betaReduce) ms
    modifying #multiset (MS.putMany ms')
applyMatchEffect (TreeReplacement []) = modifying #zipper Z.dropFocus
applyMatchEffect (TreeReplacement template) = do
    binder <- lift get
    let (rewritten, _) = runIdentity $ mapM betaReduce template `runStateT` binder
    modifying #zipper (`Z.spliceIn` concat rewritten)

-- Runtime debug printing functions
printZipper :: String -> RuntimeT IO
printZipper l = gets Runtime.verbose >>= \v -> when v $ do
    zipper <- gets Runtime.zipper
    lift $ putStr (l ++ ": ")
    lift $ print zipper
printLog :: String -> RuntimeT IO
printLog l = gets Runtime.verbose >>= \v -> when v . lift . putStrLn $ l

-- Try to apply our rewrite rules at the current rewrite head. Gives back the number of rules applied.
applyDefs :: RuntimeTV IO Int
applyDefs = do
    onceDefs <- gets Runtime.singleUseRules
    repeatDefs <- gets Runtime.rules
    printLog $ "Once defs: " ++ show onceDefs
    printLog $ "Repeat defs: " ++ show repeatDefs
    -- Grab the first single use rule that satisfies all conditions and apply it
    pocket <- gets Runtime.multiset
    printLog $ "Pocket: " ++ show pocket
    appliedOnceRule <- applyFirstMatchingDefinition onceDefs
    printLog $ "Once applied: " ++ show appliedOnceRule
    -- A single use rule matched once, we gotta delete it!
    when (isJust appliedOnceRule) $ modifying #singleUseRules (filter (/= fromJust appliedOnceRule))
    -- Grab the first multi use rule that satisfies all conditions and apply it
    appliedRule <- applyFirstMatchingDefinition repeatDefs
    printLog $ "Repeat applied: " ++ show appliedRule
    pure $ sum (bool 0 1 <$> [isJust appliedOnceRule, isJust appliedRule])
        where
            -- Fully apply the first matching definition we can find. Mutate the runtime, give back Nothing if we can't
            applyFirstMatchingDefinition :: [EatenDef] -> RuntimeTV IO (Maybe EatenDef)
            applyFirstMatchingDefinition defs = do
                runtime <- get
                let (matchedDef, binder) = tryDefinitions defs runtime `runState` emptyBinder
                case matchedDef of
                    Just def -> let
                            matchAction = mapM_ applyMatchEffect . defMatchEffect $ def
                            bang = (`runStateT` binder) $ runStateT matchAction runtime -- TODO: brother...
                            ((_, runtime'), _) = runIdentity bang
                        in do
                            put runtime'
                            pure $ Just def
                    Nothing -> pure Nothing

-- Run `applyDefs` until there's no point...
fixApplyDefs :: RuntimeTV IO Int
fixApplyDefs = do
    count <- applyDefs
    eatBuiltin
    eatDef
    if count == 0
        then pure 0
        else do
            tc <- fixApplyDefs
            pure $ count + tc

-- Carry out one step of Rosin's execution. This essentially carries out the following:
--   1) We check for a definition or a builtin at the current rewrite head and ingest it if there's one there
--   2) We try to apply our rewrite rules at the current rewrite head as many times as possible. Between steps we repeat 1, checking for defs and builtins.
--   3) We move onto the next element in the tree in DFS order. If we're at the end, we loop back to the start
-- Gives back (the amount of rules applied, whether we jumped back to the top of the tree). 
runStep :: RuntimeTV IO (Int, Bool)
runStep = do
    verbose <- gets Runtime.verbose
    when verbose (lift $ putStrLn "== STARTING STEP ==")
    printZipper "Pre-step"

    -- Begin 1
    eatDef
    eatBuiltin
    printZipper "Post-eat"

    -- Begin 2
    rulesApplied <- fixApplyDefs
    modifying #multiset cleanUp

    -- Begin 3 (I Love Laziness)
    newZipper <- gets (Z.nextDfs . Runtime.zipper)
    modifying #zipper (const newZipper)

    -- Give back the value we use to assess termination
    printZipper "Pre-termination"
    atTop <- gets ((== []) . Z._Ups . Runtime.zipper)
    when verbose . lift . putStrLn . concat $ ["Rules applied: ", show rulesApplied, "; at top? ", show atTop]
    pure (rulesApplied, atTop)


-- Run `runStep` until there's no point...
fixStep :: RuntimeT IO
fixStep = go 0
    where
        readyToStop = do
            (count, reset) <- runStep
            unless reset (if count > 0 then go count else readyToStop)
        go count = do
            (count', reset) <- runStep
            if reset
                then readyToStop
                else go $ count + count'


-- Executes a Rosin runtime
run :: Runtime -> IO Runtime
run = execStateT fixStep

emptyRuntime :: String -> Bool -> [Tree RValue] -> Runtime
emptyRuntime filepath verbose trees = Runtime filepath verbose emptyRules emptyRules (Z.zipperFromTrees trees) MS.empty
    where emptyRules = []

runEasy :: String -> Bool -> [Tree RValue] -> IO ([Tree RValue], [EatenDef])
runEasy filepath verbose inTrees = do
    out <- run (emptyRuntime filepath verbose inTrees)
    pure (unzipper . Runtime.zipper $ out, Runtime.rules out)
    where
        -- we add one layer of `Branch` in Z.zipperFromTrees, let's pop it off here
        unzipper z = case Z.treeFromZipper z of
            Branch trees -> trees
            val@(Leaf _) -> [val]