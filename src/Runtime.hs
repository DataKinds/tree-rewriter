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
import Control.Monad.Trans.State (execStateT, runStateT, mapStateT)
import Control.Monad (when, ap, join, unless)
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
import Data.Functor.Identity (runIdentity, Identity)
import Prettyprinter.Render.Terminal (putDoc, color, Color (Red))
import Optics 
import Debug.Trace (trace)
import Zipper (treeFromZipper)
import Control.Monad.State.Class
import Control.Monad.State.Lazy (StateT)


bumpEpoch :: MonadRuntime r m => m ()
bumpEpoch = modifying (runtime % #epoch) (\num -> if num == defaultTag - 1 then num + 2 else num + 1)


-- | Apply matching conditions from a definition according to a given runtime.
-- False if we fail to apply a condition. True if they all apply.
tryBindConditions :: MonadBinder b m
                  => [MatchCondition] -> Runtime -> m Bool -- True if all conditions applied
tryBindConditions [] _ = pure True
tryBindConditions (cond:xs) r = do
    success <- applyMatchCondition cond r
    go <- tryBindConditions xs r
    pure $ success && go

subprocess :: (MonadRuntime r m, MonadIO m) => [Tree RValue] -> m (Tree RValue)
-- should run an existing runtime on an input tree until it terminates
-- at which time, it should return how it modified the tree it passed in
-- then return the runtime with just the bag and rule state modified
subprocess input = do
    prevZipper <- use (runtime % #zipper)
    prevEmptyCycle <- use (runtime % #emptyCycle)
    prevEmptyCycleCount <- use (runtime % #emptyCycleCount)
    assign (runtime % #zipper) (Z.zipperFromTrees defaultTag input)
    assign (runtime % #emptyCycle) True -- see RuntimeTypes.emptyRuntime
    assign (runtime % #emptyCycleCount) 0
    _ <- runStep 
    transformedZipper <- use (runtime % #zipper)
    assign (runtime % #zipper) prevZipper
    assign (runtime % #emptyCycle) prevEmptyCycle
    assign (runtime % #emptyCycleCount) prevEmptyCycleCount
    return . treeFromZipper $ transformedZipper

-- | Apply a MatchEffect to a given runtime, monadically
-- Sequence with a successful `applyMatchCondition` to mutate the runtime state based on a definition -- apply a rule
applyMatchEffect :: (MonadIO m, MonadRuntime s m) => MatchEffect -> m ()
applyMatchEffect (Force pvar) = do
    -- it's gonna be this: getTreeBinding name to grab the tree in question
    -- then save the current runtime zipper along with the current epoch/empty cycle/empty cycle count, to be restored later
    -- swap the whole ahh zipper out for the binding we just grabbed
    -- call runStep ourselves (don't you love coroutines?)
    -- save the upmost of the new runtime zipper to the name binding via addTreeBinding name (TODO: we want to REPLACE this binding not add to it)
    -- then restore all that state we saved in the first step
    let assertJust = fromMaybe (error $ "binding " ++ show pvar ++ " forced but doesn't exist")
    treeToForce <- getTreeBinding pvar
    forcedTree <- subprocess [assertJust treeToForce]
    void $ setTreeBinding pvar forcedTree
applyMatchEffect (MultisetPush ms) = do
    ms' <- MS.traverseValues betaReduce ms
    bumpEpoch -- pushing to the multiset bumps the epoch number
    modifying (runtime % #multiset) (MS.putMany ms')
applyMatchEffect (TreeReplacement []) = modifying (runtime % #zipper) Z.dropFocus -- TODO: is this line OK? check what empty replacements do
applyMatchEffect (TreeReplacement template) = do
    -- bdr <- use binder
    -- goodTag <- use (runtime % #epoch)
    -- let (rewritten, _) = runIdentity $ runStateT (mapM betaReduce template) bdr
        -- tagged = tagAll goodTag <$> rewritten
    rewritten <- mapM betaReduce template
    modifying (runtime % #zipper) (`Z.spliceIn` rewritten)

-- | Apply all the effects from a given rule
applyRuleEffects :: (MonadIO m, MonadRuntime s m) => MatchRule -> m ()
applyRuleEffects = mapM_ applyMatchEffect . matchEffect

treeMapReduce :: Semigroup a => (Tree b -> a) -> Tree b -> a
treeMapReduce mapper input@(Leaf _ _) = mapper input
treeMapReduce mapper input@(Branch _ xs) = sconcat $ mapper input :| (treeMapReduce mapper <$> xs)


-- | Bind variables associated with a match condition, or fail out and return False
applyMatchCondition :: MonadBinder b m => MatchCondition -> Runtime -> m Bool
applyMatchCondition (MultisetPattern ms) r = let
    pocket = runtimeMultiset r
    in do
        ms' <- MS.traverseValues betaReduce ms
        pure $ MS.allInside ms' pocket -- TODO: pattern match!
applyMatchCondition (TreePattern pat) r = get >>= \binding -> let -- TODO: beta reduce, in case this condition comes after the multiset
    subject = Z.look . runtimeZipper $ r
    -- epoch = r ^. #epoch
    -- canApplyDelayed tree = trace ("CHECKING EPOCH " ++ show epoch ++ " ON TREE " ++ show tree) $ allTags (== epoch) tree
    in tryApply (const False) subject pat


-- | Grab the first matching rule out of a list of rules. Apply it and tag the tree and modify the runtime accordingly.
-- If we couldn't find a matching rule from the input list, give back Nothing.
applyRule :: (MonadIO m, MonadRuntime s m) => [MatchRule] -> m (Maybe MatchRule)
applyRule [] = pure Nothing
applyRule (rule:rest) = do
    applied <- tryRule rule
    if applied
    then pure $ Just rule
    else applyRule rest
    where
        -- | try applying one MatchRule, and apply its effects iff it succeeds 
        tryRule :: (MonadIO m, MonadRuntime s m) => MatchRule -> m Bool
        tryRule rule' = do
            r <- use runtime
            applied <- nullBinder >> tryBindConditions (matchCondition rule') r 
            when applied $ applyRuleEffects rule'
            pure applied 
    

-- | Add a new tree rewriting rule into the runtime
addRule :: MonadRuntime r m => MatchRule -> m ()
addRule rule = case useCount rule of
    UseOnce -> modifying (runtime % #singleUseRules) (rule:)
    UseMany -> modifying (runtime % #rules) (rule:)

-- | Check the current position of the rewrite head. If it's pointing to a definition, consume it.
-- Gives back the count of definitions consumed.
eatDef :: MonadRuntime r m => m Int
eatDef = do
    subject <- use (runtime % #zipper % to Z.look)
    case recognizeDef subject of
        -- Add the rule definition to the runtime and snip it out from the input tree
        Just td -> do
            addRule td
            bumpEpoch
            modifying (runtime % #zipper) (Z.nextDfs . Z.dropFocus)
            pure 1
        Nothing -> pure 0


-- | Check the current position of the rewrite head. If it's pointing to a builtin, rewrite it and execute any effects.
eatBuiltin :: (MonadRuntime r m, MonadIO m) => m Int
eatBuiltin = do
    subject <- use (runtime % #zipper % to Z.look)
    case recognizeBuiltin subject of
        Just (BuiltinRule name args) -> do 
            dispatchBuiltin args name
            pure 1
        Nothing -> pure 0
    where
        -- | Execute a given builtin. The `args` passed in are the args passed to the builtin.
        dispatchBuiltin :: (MonadRuntime r m, MonadIO m) => [Tree RValue] -> T.Text -> m ()
        dispatchBuiltin args = \case
            "version" -> modifying (runtime % #zipper) (`Z.put` str "v0.0.0. That's right, We Aren't Semver Yet!")
            "bag" -> do
                bag <- uses (runtime % #multiset) (branch . map (\(x, n) -> branch [x, num n]) . MS.toList)
                modifying (runtime % #zipper) (`Z.put` bag)
            "getLine" -> do
                line <- liftIO TIO.getLine
                modifying (runtime % #zipper) (`Z.put` tstr line)
            "print" -> do
                let printer = \case
                        LeafStr input -> TIO.putStrLn input
                        other -> print other
                liftIO $ mapM_ printer args
                modifying (runtime % #zipper) (Z.nextDfs . Z.dropFocus)
            "parse" -> case args of
                LeafStr input:_ -> do
                    filepath <- use (runtime % #path)
                    case parse (T.unpack input) (filepath++"<eval>") of
                        Left err -> modifying (runtime % #zipper) (`Z.put` (tstr . T.pack $ "parse error: " ++ show err))
                        Right success -> modifying (runtime % #zipper) (`Z.spliceRight` success)
                _ -> pure ()
            "cat" -> case args of
                LeafStr path:_ -> do
                    pathContext <- use (runtime % #path % to takeDirectory)
                    fileContents <- liftIO . TIO.readFile . (pathContext </>) . T.unpack $ path
                    modifying (runtime % #zipper) (`Z.put` tstr fileContents)
                _ -> pure ()
            shouldntBePossible -> error$"Unrecognized builtin "++T.unpack shouldntBePossible++" that matched -- please report this as a bug!"

-- Runtime debug printing functions
whenVerbose :: MonadRuntime r m => m () -> m ()
whenVerbose f = use (runtime % #verbose) >>= flip when f

printLog :: (MonadRuntime r m, MonadIO m) => String -> m ()
printLog = whenVerbose . liftIO . putStrLn 

printZipper :: (MonadRuntime r m, MonadIO m) => m ()
printZipper = whenVerbose $ do
    liftIO $ putStrLn "Zipper:"
    z <- use (runtime % #zipper)
    liftIO . putDoc . uncurry (prettyTreeWithFocus (color Red)) . (Z.look &&& Z.look . Z.upmost) $ z
    liftIO $ putStrLn ""

printRuntime :: (MonadRuntime r m, MonadIO m) => m ()
printRuntime = whenVerbose $ use runtime >>= liftIO . putStr . prettyRuntime

printRunSeparator :: (MonadRuntime r m, MonadIO m) => m ()
printRunSeparator = whenVerbose (liftIO $ putStrLn "======================================")

printSectionSeparator :: (MonadRuntime r m, MonadIO m) => m ()
printSectionSeparator = whenVerbose (liftIO $ putStrLn "------------------------")


-- Try to apply our rewrite rules at the current rewrite head. Gives back the number of rules applied.
applyDefs :: (MonadRuntime r m, MonadIO m) => m Int
applyDefs = do
    onceDefs <- use (runtime % #singleUseRules)
    repeatDefs <- use (runtime % #rules)
    -- Grab the first single use rule that satisfies all conditions and apply it
    appliedOnceRule <-  applyRule onceDefs
    -- A single use rule matched once, we gotta delete it!
    for_ appliedOnceRule $ \rule -> do 
        printLog $ "Applied one-time rule: " ++ prettyMatchRule rule
        modifying (runtime % #singleUseRules) (filter (/= rule))
    -- Grab the first multi use rule that satisfies all conditions and apply it
    appliedRule <- applyRule repeatDefs
    for_ appliedRule $ \rule -> printLog $ "Applied rule: " ++ prettyMatchRule rule
    pure $ sum (bool 0 1 . isJust <$> [appliedOnceRule, appliedRule])
    -- where
    --     discardBinder :: (MonadRuntime r m) => RuntimeM (StateT Binder Identity) a -> RuntimeM m a
    --     discardBinder = mapStateT (pure . fst . flip runBinder emptyBinder)

-- Run `applyDefs` until there's no point...
fixApplyDefs :: (MonadRuntime r m, MonadIO m) => m Int
fixApplyDefs = do
    n <- applyDefs
    if n == 0
        then pure 0
        else do
            modifying (runtime % #multiset) cleanUp
            tc <- fixApplyDefs
            pure $ n + tc

-- Carry out one step of Rosin's execution, then tail call if we are to continue execution.
-- Gives back the amount of rules applied in the final step (this should ALWAYS be 0!)
runStep :: (MonadRuntime r m, MonadIO m) => m Int
-- runStep :: StateT Runtime IO Int
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
        epoch <- use (runtime % #epoch)
        modifying (runtime % #zipper) (Z.updateFocus $ set tag epoch) -- TODO: I don't think this is right? But Idk maybe it is?
        -- Matching stuff usually moves our zipper forward, so we only move forward if we didn't match on anything.
        modifying (runtime % #zipper) Z.nextDfs
    -- If we matched on anything, mark the execution as not finished and bump the epoch number by one.
    when (rulesApplied /= 0) $ do
        assign (runtime % #emptyCycle) False
        assign (runtime % #emptyCycleCount) 0
        modifying (runtime % #epoch) (+ 1) 

    printZipper
    -- Let's stop executing if our "done" flag is set and we're back at the top of the input tree
    -- Conditions to stop execution:
        -- We're at the top of the input tree (aka, we've just looped through it)
        -- We've already cycled through the input tree 2 times without applying any rule
    atTop <- use (runtime % #zipper % to Z._Ups % to (== []))
    wasEmptyCycle <- use (runtime % #emptyCycle)
    emptyCycleCount <- use (runtime % #emptyCycleCount)
    case (atTop, wasEmptyCycle, emptyCycleCount) of
        (False, _, _) -> runStep
        (True, True, 2) -> pure rulesApplied
        (True, True, _) -> modifying (runtime % #emptyCycleCount) (+ 1) >> runStep
        (True, False, _) -> assign (runtime % #emptyCycleCount) 0 >> assign (runtime % #emptyCycle) True >> runStep

-- | Sets up the Runtime to take control of its own execution through runStep.
firstStep :: (MonadRuntime r m, MonadIO m) => m ()
firstStep = assign (runtime % #emptyCycle) True >> assign (runtime % #epoch) 0 >> void runStep



--- Concrete initialization of the Runtime monad stack ---

-- | Executes a Rosin runtime
run :: Runtime -> IO Runtime
run r = fst <$> execStateT firstStep (r, emptyBinder)

-- we add one layer of `Branch` in Z.zipperFromTrees, let's pop it off here
unzipper :: Z.Zipper a -> [Tree a]
unzipper z = case Z.treeFromZipper z of
    Branch _ trees -> trees
    val@(Leaf _ _) -> [val]

runEasy :: String -> Bool -> [Tree RValue] -> IO ([Tree RValue], [MatchRule])
runEasy filepath verbose inTrees = do
    out <- run (emptyRuntime filepath verbose inTrees)
    pure (unzipper . runtimeZipper $ out, runtimeRules out)
  