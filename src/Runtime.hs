{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Runtime where

import Core
import Core.DSL ( str, num, branch, tstr )
import RuntimeTypes
import qualified Zipper as Z
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (gets, execStateT, runStateT, mapStateT, get, put)
import Control.Monad (when, ap)
import Data.Maybe (isJust, fromJust, fromMaybe)
import qualified Multiset as MS
import Recognizers 
import System.FilePath ((</>), takeDirectory)
import Parser (parse)
import Data.Bool (bool)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Multiset (cleanUp)
import Optics.State
import Data.Functor (void)
import Data.Semigroup 
import Data.List.NonEmpty (NonEmpty (..))
import Data.Foldable (for_)
import Control.Arrow ((&&&))
import Data.Functor.Identity (runIdentity)
import Prettyprinter.Render.Terminal (putDoc, color, Color (Red))
import Optics 
import Debug.Trace (trace)
import Zipper (treeFromZipper)


bumpEpoch :: Monad m => RuntimeM m ()
bumpEpoch = modifying #epoch (\num -> if num == defaultTag - 1 then num + 2 else num + 1)


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
tryRules :: [MatchRule] -> Runtime -> Maybe (Binder_ MatchRule)
tryRules rules r = let
    go :: [MatchRule] -> Binder_ (Maybe MatchRule)
    go [] = pure Nothing
    go (rule:rs) = do
        success <- put emptyBinder >> tryBindConditions (matchCondition rule) r
        if success 
            then pure $ Just rule
            else go rs
    (maybeRule, binder) = runBinder (go rules) emptyBinder
    in maybeRule >>= (\rule -> Just $ put binder >> pure rule)

subprocess :: [Tree RValue] -> RuntimeM IO (Tree RValue)
-- should run an existing runtime on an input tree until it terminates
-- at which time, it should return how it modified the tree it passed in
-- then return the runtime with just the bag and rule state modified
subprocess input = do
    prevZipper <- use #zipper
    prevEmptyCycle <- use #emptyCycle
    prevEmptyCycleCount <- use #emptyCycleCount
    assign #zipper (Z.zipperFromTrees defaultTag input)
    assign #emptyCycle True -- see RuntimeTypes.emptyRuntime
    assign #emptyCycleCount 0
    _ <- runStep 
    transformedZipper <- use #zipper
    assign #zipper prevZipper
    assign #emptyCycle prevEmptyCycle
    assign #emptyCycleCount prevEmptyCycleCount
    return . treeFromZipper $ transformedZipper

-- | Apply a MatchEffect to a given runtime, monadically
-- Sequence with a successful `applyMatchCondition` to mutate the runtime state based on a definition -- apply a rule
applyMatchEffect :: MatchEffect -> RuntimeM (BinderT IO) ()
applyMatchEffect (Force pvar) = do
    -- it's gonna be this: getTreeBinding name to grab the tree in question
    -- then save the current runtime zipper along with the current epoch/empty cycle/empty cycle count, to be restored later
    -- swap the whole ahh zipper out for the binding we just grabbed
    -- call runStep ourselves (don't you love coroutines?)
    -- save the upmost of the new runtime zipper to the name binding via addTreeBinding name (TODO: we want to REPLACE this binding not add to it)
    -- then restore all that state we saved in the first step
    treeToForce <- fmap (fromMaybe (error $ "binding " ++ show pvar ++ " forced but doesn't exist")) . lift . getTreeBinding $ pvar
    forcedTree <- subprocess treeToForce
    lift $ overwriteTreeBinding pvar forcedTree

applyMatchEffect (MultisetPush ms) = do
    ms' <- lift $ MS.traverseValues (fmap (rebranch defaultTag)  . betaReduce) ms
    bumpEpoch -- pushing to the multiset bumps the epoch number
    modifying #multiset (MS.putMany ms')
applyMatchEffect (TreeReplacement []) = modifying #zipper Z.dropFocus
applyMatchEffect (TreeReplacement template) = do
    binder <- lift get
    goodTag <- gets runtimeEpoch
    let (rewritten, _) = runIdentity $ mapM betaReduce template `runStateT` binder
        tagged = tagAll goodTag <$> concat rewritten
    modifying #zipper (`Z.spliceIn` tagged)

-- | Apply all the effects from a given rule
applyRuleEffects :: MatchRule -> RuntimeM Binder_ ()
applyRuleEffects = mapM_ applyMatchEffect  . matchEffect

treeMapReduce :: Semigroup a => (Tree b -> a) -> Tree b -> a
treeMapReduce mapper input@(Leaf _ _) = mapper input
treeMapReduce mapper input@(Branch _ xs) = sconcat $ mapper input :| (treeMapReduce mapper <$> xs)


-- | Bind variables associated with a match condition, or fail out and return False
applyMatchCondition :: MatchCondition -> Runtime -> Binder_ Bool
applyMatchCondition (MultisetPattern ms) r = let
    pocket = runtimeMultiset r
    in do
        ms' <- MS.traverseValues (fmap (rebranch defaultTag) . betaReduce) ms
        pure $ MS.allInside ms' pocket -- TODO: pattern match!
applyMatchCondition (TreePattern pat) r = get >>= \binding -> let -- TODO: beta reduce, in case this condition comes after the multiset
    subject = Z.look . runtimeZipper $ r
    epoch = r ^. #epoch
    canApplyDelayed tree = trace ("CHECKING EPOCH " ++ show epoch ++ " ON TREE " ++ show tree) $ allTags (== epoch) tree
    in hoistState $ tryApply (not . canApplyDelayed) subject pat


-- | Grab the first matching rule out of a list of rules. Apply it and tag the tree and modify the runtime accordingly.
-- If we couldn't find a matching rule from the input list, give back Nothing.
applyRule :: [MatchRule] -> RuntimeM Binder_ (Maybe MatchRule)
applyRule rules = do
    runtime <- get
    maybe (pure Nothing) (fmap Just . handleMatchedRule) (tryRules rules runtime)
    where
        handleMatchedRule :: Binder_ MatchRule -> RuntimeM Binder_ MatchRule
        handleMatchedRule ruleWithBindings = do
            let (matchedRule, binder) = runBinder ruleWithBindings emptyBinder
            lift $ put binder
            applyRuleEffects matchedRule >> pure matchedRule
    

-- | Add a new tree rewriting rule into the runtime
addRule :: Monad m => MatchRule -> RuntimeM m ()
addRule rule = case useCount rule of
    UseOnce -> modifying #singleUseRules (rule:)
    UseMany -> modifying #rules (rule:)

-- | Check the current position of the rewrite head. If it's pointing to a definition, consume it.
-- Gives back the count of definitions consumed.
eatDef :: Monad m => RuntimeM m Int
eatDef = do
    subject <- gets (Z.look . runtimeZipper)
    case recognizeDef subject of
        -- Add the rule definition to the runtime and snip it out from the input tree
        Just td -> do
            addRule td
            bumpEpoch
            modifying #zipper (Z.nextDfs . Z.dropFocus)
            pure 1
        Nothing -> pure 0


-- | Check the current position of the rewrite head. If it's pointing to a builtin, rewrite it and execute any effects.
eatBuiltin :: RuntimeM IO Int
eatBuiltin = do
    subject <- gets (Z.look . runtimeZipper)
    case recognizeBuiltin subject of
        Just (BuiltinRule name args) -> do 
            dispatchBuiltin args name
            pure 1
        Nothing -> pure 0
    where
        -- | Execute a given builtin. The `args` passed in are the args passed to the builtin.
        dispatchBuiltin :: [Tree RValue] -> T.Text -> RuntimeM IO ()
        dispatchBuiltin args = \case
            "version" -> modifying #zipper (`Z.put` str "v0.0.0. That's right, We Aren't Semver Yet!")
            "bag" -> do
                bag <- gets (branch . map (\(x, n) -> branch [x, num n]) . MS.toList . runtimeMultiset)
                modifying #zipper (`Z.put` bag)
            "getLine" -> do
                line <- liftIO TIO.getLine
                modifying #zipper (`Z.put` tstr line)
            "print" -> do
                let printer = \case
                        LeafStr input -> TIO.putStrLn input
                        other -> print other
                liftIO $ mapM_ printer args
                modifying #zipper (Z.nextDfs . Z.dropFocus)
            "parse" -> case args of
                LeafStr input:_ -> do
                    filepath <- gets runtimePath
                    case parse (T.unpack input) (filepath++"<eval>") of
                        Left err -> modifying #zipper (`Z.put` (tstr . T.pack $ "parse error: " ++ show err))
                        Right success -> modifying #zipper (`Z.spliceRight` success)
                _ -> pure ()
            "cat" -> case args of
                LeafStr path:_ -> do
                    pathContext <- gets (takeDirectory . runtimePath)
                    fileContents <- lift . TIO.readFile . (pathContext </>) . T.unpack $ path
                    modifying #zipper (`Z.put` tstr fileContents)
                _ -> pure ()
            shouldntBePossible -> error$"Unrecognized builtin "++T.unpack shouldntBePossible++" that matched -- please report this as a bug!"

-- Runtime debug printing functions
whenVerbose f = gets runtimeVerbose >>= flip when f

printLog :: String -> RuntimeM IO ()
printLog = whenVerbose . lift . putStrLn 

printZipper :: RuntimeM IO ()
printZipper = whenVerbose $ do
    lift $ putStrLn "Zipper:"
    z <- use #zipper
    lift . putDoc . uncurry (prettyTreeWithFocus (color Red)) . (Z.look &&& Z.look . Z.upmost) $ z
    lift $ putStrLn ""

printRuntime :: RuntimeM IO ()
printRuntime = whenVerbose $ get >>= lift . putStr . prettyRuntime

printRunSeparator :: RuntimeM IO ()
printRunSeparator = whenVerbose (lift $ putStrLn "======================================")

printSectionSeparator :: RuntimeM IO ()
printSectionSeparator = whenVerbose (lift $ putStrLn "------------------------")


-- Try to apply our rewrite rules at the current rewrite head. Gives back the number of rules applied.
applyDefs :: RuntimeM IO Int
applyDefs = do
    onceDefs <- gets runtimeSingleUseRules
    repeatDefs <- gets runtimeRules
    -- Grab the first single use rule that satisfies all conditions and apply it
    appliedOnceRule <- discardBinder $ applyRule onceDefs
    -- A single use rule matched once, we gotta delete it!
    for_ appliedOnceRule $ \rule -> do 
        printLog $ "Applied one-time rule: " ++ prettyMatchRule rule
        modifying #singleUseRules (filter (/= rule))
    -- Grab the first multi use rule that satisfies all conditions and apply it
    appliedRule <- discardBinder $ applyRule repeatDefs
    for_ appliedRule $ \rule -> printLog $ "Applied rule: " ++ prettyMatchRule rule
    pure $ sum (bool 0 1 . isJust <$> [appliedOnceRule, appliedRule])
    where
        discardBinder :: Monad m => RuntimeM Binder_ a -> RuntimeM m a
        discardBinder = mapStateT (pure . fst . flip runBinder emptyBinder)

-- Run `applyDefs` until there's no point...
fixApplyDefs :: RuntimeM IO Int
fixApplyDefs = do
    n <- applyDefs
    if n == 0
        then pure 0
        else do
            modifying #multiset cleanUp
            tc <- fixApplyDefs
            pure $ n + tc

-- Carry out one step of Rosin's execution, then tail call if we are to continue execution.
-- Gives back the amount of rules applied in the final step (this should ALWAYS be 0!)
runStep :: RuntimeM IO Int
runStep = do
    printRunSeparator
    printZipper
    printSectionSeparator
    printRuntime
    printSectionSeparator

    -- Check for a definition or a builtin at the current rewrite head, ingest it if there's one there.
    -- Tries to apply user rewrite rules as many times as possible, as user-made rules can output other rules
    -- TODO: are there any builtins that can also produce other rules? can this break certain structures if they're misinterpreted?
    count <- fixApplyDefs
    count' <- eatBuiltin -- can do the following zipper moves: put, nextDfs . dropFocus, spliceRight
    count'' <- fixApplyDefs
    count''' <- eatDef -- can do the following zipper moves: put, nextDfs . dropFocus

    let rulesApplied = count + count' + count'' + count'''
    when (rulesApplied == 0) $ do
        -- 3.3: If no rules could be applied, **tag** all nodes in the **pointer**'s **subtree** with the **epoch number**
        epoch <- use #epoch
        modifying #zipper (Z.updateFocus $ set tag epoch) -- TODO: I don't think this is right? But Idk maybe it is?
        -- Matching stuff usually moves our zipper forward, so we only move forward if we didn't match on anything.
        modifying #zipper Z.nextDfs
    -- If we matched on anything, mark the execution as not finished and bump the epoch number by one.
    when (rulesApplied /= 0) $ do
        assign #emptyCycle False
        assign #emptyCycleCount 0
        modifying #epoch (+ 1) 

    printZipper
    -- Let's stop executing if our "done" flag is set and we're back at the top of the input tree
    -- Conditions to stop execution:
        -- We're at the top of the input tree (aka, we've just looped through it)
        -- We've already cycled through the input tree 2 times without applying any rule
    atTop <- gets ((== []) . Z._Ups . runtimeZipper)
    wasEmptyCycle <- use #emptyCycle
    emptyCycleCount <- use #emptyCycleCount
    case (atTop, wasEmptyCycle, emptyCycleCount) of
        (False, _, _) -> runStep
        (True, True, 2) -> pure rulesApplied
        (True, True, _) -> modifying #emptyCycleCount (+ 1) >> runStep
        (True, False, _) -> assign #emptyCycleCount 0 >> assign #emptyCycle True >> runStep

-- | Sets up the Runtime to take control of its own execution through runStep.
firstStep :: RuntimeM IO ()
firstStep = assign #emptyCycle True >> assign #epoch 0 >> void runStep

-- | Executes a Rosin runtime
run :: Runtime -> IO Runtime
run = execStateT firstStep

-- we add one layer of `Branch` in Z.zipperFromTrees, let's pop it off here
unzipper :: Z.Zipper a -> [Tree a]
unzipper z = case Z.treeFromZipper z of
    Branch _ trees -> trees
    val@(Leaf _ _) -> [val]

runEasy :: String -> Bool -> [Tree RValue] -> IO ([Tree RValue], [MatchRule])
runEasy filepath verbose inTrees = do
    out <- run (emptyRuntime filepath verbose inTrees)
    pure (unzipper . runtimeZipper $ out, runtimeRules out)
  