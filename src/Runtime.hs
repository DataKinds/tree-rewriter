{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Runtime where
    
import Core
import qualified Zipper as Z
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (runStateT), modify, gets, execStateT, State, runState, state, mapStateT, get)
import Control.Monad (unless, when, void)
import Data.Maybe (isJust, fromJust, catMaybes, mapMaybe, listToMaybe, isNothing)
import qualified Data.Map as M
import Data.Function (on)
import qualified Multiset as MS 
import Recognizers (recognizeDef, recognizeBuiltin, BuiltinRule (..))
import Data.Bifunctor (Bifunctor(bimap, second))
import System.FilePath (makeRelative, (</>), takeDirectory)
import Parser (parse)
import Data.Bool (bool)
import Debug.Trace (trace)
import Definitions (MatchCondition (..), EatenDef, UseCount (..), MatchEffect (..), defUseCount, defMatchCondition)
import Core (BinderT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (find)
import Data.List (findIndex)


-- Runtime handles the state of the rewrite head processing the input data 
data Runtime = Runtime {
    -- What file are we executing
    runtimePath :: String,
    -- Do we print out debug information?
    runtimeVerbose :: Bool,
    -- What rewriting rules are active?
    runtimeRules :: [EatenDef],
    -- What rewriting lambdas are active?
    runtimeSingleUseRules :: [EatenDef],
    -- Where are we in the data tree?
    runtimeZipper :: Z.Zipper RValue,
    -- Multiset state!
    runtimeMultiset :: MS.Multiset (Tree RValue) 
} deriving (Show)
type RuntimeTV m = StateT Runtime m
type RuntimeT m = RuntimeTV m ()
type RuntimeV = State Runtime

-- Runtime lenses -- 
updateRuntimeRules :: ([EatenDef] -> [EatenDef]) -> Runtime -> Runtime
updateRuntimeRules f b = b { runtimeRules = f (runtimeRules b) }
updateRuntimeSingleUseRules :: ([EatenDef] -> [EatenDef]) -> Runtime -> Runtime
updateRuntimeSingleUseRules f b = b { runtimeSingleUseRules = f (runtimeSingleUseRules b) }
updateRuntimeZipper :: (Z.Zipper RValue -> Z.Zipper RValue) -> Runtime -> Runtime
updateRuntimeZipper f b = b { runtimeZipper = f (runtimeZipper b) }
updateRuntimeMultiset :: (MS.Multiset (Tree RValue) -> MS.Multiset (Tree RValue)) -> Runtime -> Runtime
updateRuntimeMultiset f b = b { runtimeMultiset = f (runtimeMultiset b) }

-- Runtime lens state modifiers --
modifyRuntimeRules :: Monad m => ([EatenDef] -> [EatenDef]) -> RuntimeT m
modifyRuntimeRules = modify . updateRuntimeRules
modifyRuntimeSingleUseRules :: Monad m => ([EatenDef] -> [EatenDef]) -> RuntimeT m
modifyRuntimeSingleUseRules = modify . updateRuntimeSingleUseRules
modifyRuntimeZipper :: Monad m => (Z.Zipper RValue -> Z.Zipper RValue) -> RuntimeT m
modifyRuntimeZipper = modify . updateRuntimeZipper
modifyRuntimeMultiset :: Monad m => (MS.Multiset (Tree RValue) -> MS.Multiset (Tree RValue)) -> RuntimeT m
modifyRuntimeMultiset = modify . updateRuntimeMultiset

-- Add a new tree rewriting rule into the runtime
addRule :: Monad m => EatenDef -> RuntimeT m
addRule rule = modifyRuntimeRules (rule:)

-- Add a new single use tree rewriting rule into the runtime
addSingleUseRule :: Monad m => EatenDef -> RuntimeT m
addSingleUseRule rule = modifyRuntimeSingleUseRules (rule:)


-- Parse and ingest definitions --

-- defUseCount :: EatenDef -> UseCount
-- defUseCount = \case 
--     TreeDef uc _ _ -> uc
--     MultisetDef uc _ -> uc
--     ComboDef uc _ _ _ -> uc

-- defToRewrite :: EatenDef -> Maybe (Rewrite RValue)
-- defToRewrite (TreeDef _ pat templates) = Just $ Rewrite pat templates
-- defToRewrite (ComboDef _ _ pat templates) = Just $ Rewrite pat templates
-- defToRewrite _ = Nothing

-- -- What, if any, patterns does this definition want to match out of the multiset bag?
-- defWantsFromBag :: EatenDef -> MS.Multiset (Tree RValue)
-- defWantsFromBag (MultisetDef _ ma) = multisetPops ma
-- defWantsFromBag (ComboDef _ ma _ _) = multisetPops ma
-- defWantsFromBag _ = MS.empty

-- -- What, if any, patterns does this definition want to put into the multiset bag?
-- defPutsInBag :: EatenDef -> MS.Multiset (Tree RValue)
-- defPutsInBag (MultisetDef _ ma) = multisetPushs ma
-- defPutsInBag (ComboDef _ ma _ _) = multisetPushs ma
-- defPutsInBag _ = MS.empty

-- Execute a Rosin runtime --

-- Check the current position of the rewrite head. If it's pointing to a definition, consume it.
eatDef :: Monad m => RuntimeT m
eatDef = do
    subject <- gets (Z.look . runtimeZipper)
    case recognizeDef subject of 
        -- Add the rule definition to the runtime and snip it out from the input tree
        Just td -> case defUseCount td of
            UseOnce -> do
                addSingleUseRule td
                modifyRuntimeZipper Z.dropFocus
            UseMany -> do
                addRule td
                modifyRuntimeZipper (`Z.put` branch [sym "defined", str $ show td])
        Nothing -> pure ()

-- Execute a given builtin
dispatchBuiltin :: [Tree RValue] -> T.Text -> RuntimeT IO
dispatchBuiltin args = \case
    "version" -> modifyRuntimeZipper (`Z.put` str "v0.0.0. That's right, We Aren't Semver Yet!")
    "bag" -> do
        bag <- gets (branch . map (\(x, n) -> branch [x, num n]) . MS.toList . runtimeMultiset)
        modifyRuntimeZipper (`Z.put` bag)
    "parse" -> case args of
        Leaf (RString input):_ -> do
            filepath <- gets runtimePath
            case parse (T.unpack input) (filepath++"<eval>") of
                Left err -> modifyRuntimeZipper (`Z.put` (Leaf . RString . T.pack $ "parse error: " ++ show err))
                Right success -> modifyRuntimeZipper (`Z.spliceRight` success)
        _ -> pure ()
    "cat" -> case args of
        Leaf (RString path):_ -> do
            pathContext <- gets (takeDirectory . runtimePath)
            fileContents <- lift . TIO.readFile . (pathContext </>) . T.unpack $ path
            modifyRuntimeZipper (`Z.put` (Leaf . RString $ fileContents))
        _ -> pure ()
    shouldntBePossible -> error$"Unrecognized builtin "++T.unpack shouldntBePossible++" that matched -- please report this as a bug!"

-- Check the current position of the rewrite head. If it's pointing to a builtin, rewrite it and execute any effects.
eatBuiltin :: RuntimeT IO
eatBuiltin = do
    subject <- gets (Z.look . runtimeZipper)
    case recognizeBuiltin subject of 
        Just (BuiltinRule name args) -> dispatchBuiltin args name
        Nothing -> pure ()

thrd :: (a, b, c) -> c
thrd (_,_,c) = c

hoistState :: (Monad m) => State s a -> StateT s m a
hoistState = state . runState

-- Bind variables associated with a match condition, or fail out and return False
applyMatchCondition :: MatchCondition -> Runtime -> Binder_ Bool
applyMatchCondition (MultisetPattern ms) r = let
    pocket = runtimeMultiset r
    in pure $ MS.allInside (MS.fromList . map (,1) . unbranch $ ms) pocket -- TODO: pattern match!
applyMatchCondition (TreePattern pat) r = let
    rules = runtimeRules r
    subject = Z.look . runtimeZipper $ r
    in hoistState $ tryApply (const (Rules []) rules) subject pat
        -- TODO: First arg to tryApply breaks eager evaluation -- we gotta do it at the Definition level I think

-- Sequence with a successful `applyMatchCondition` to mutate the runtime state based on a definition.
applyMatchEffect :: MatchEffect -> BinderT (RuntimeTV IO) ()
applyMatchEffect (MultisetPush ms) = lift $ modifyRuntimeMultiset (MS.putMany ms)
applyMatchEffect (TreeReplacement template) = do
    rewritten <- mapStateT liftIO $ mapM betaReduce template
    lift $ modifyRuntimeZipper (`Z.spliceIn` concat rewritten)

-- Tries to apply all given rules at the focus of the runtime zipper, regardless of the current multiset state.
-- Gives back a rule if it was applied. May mutate the zipper AND the multiset state.
-- applyTreeDefs :: [EatenDef] -> RuntimeTV IO (Maybe (Rewrite RValue))
-- applyTreeDefs defs = do
--     subject <- gets (Z.look . runtimeZipper)
--     let treeDefs = filter (isJust . defToRewrite) defs
--         treeRules = mapMaybe defToRewrite defs
--     rewriteTries <- lift $ mapM (apply subject) treeRules
--     let defsWithTries = zipWith (\rw (tree, success) -> (rw, tree, success)) treeDefs rewriteTries
--     case listToMaybe $ dropWhile (not . thrd) defsWithTries of
--         Nothing -> pure Nothing
--         Just (def, rewritten, _) -> do
--             modifyRuntimeZipper (`Z.spliceIn` rewritten)
--             -- TODO: should do variable binding here! maybe we match multisets by converting to (element, count) pairs... hmm...
--             modifyRuntimeMultiset (MS.grabManyRaw (defWantsFromBag def))
--             modifyRuntimeMultiset (MS.putMany (defPutsInBag def))
--             pure . defToRewrite $ def

-- Tries to apply all given multiset-only rules, regardless of the current multiset state.
-- Gives back the def if it was applied
-- applyMultisetDefs :: [EatenDef] -> RuntimeTV IO (Maybe EatenDef)
-- applyMultisetDefs defs =
--     case filter isMultisetDef defs of 
--         [] -> pure Nothing
--         bagDef:_ -> do
--             modifyRuntimeMultiset (MS.grabManyRaw (defWantsFromBag bagDef)) -- TODO: pattern fun
--             modifyRuntimeMultiset (MS.putMany (defPutsInBag bagDef)) -- TODO: pattern fun
--             pure $ Just bagDef 

-- Filters a list of defs down to only those which are satisfied by the current state of the multiset
-- filterByMultiset :: Monad m => [EatenDef] -> RuntimeTV m [EatenDef]
-- filterByMultiset defs = do
--     pocket <- gets runtimeMultiset 
--     pure $ filter (ok pocket) defs
--     where 
--         ok :: MS.Multiset (Tree RValue) -> EatenDef -> Bool
--         ok ms def = MS.allInside (defWantsFromBag def) ms


-- Carry out one step of Rosin's execution. This essentially carries out the following:
--   1) We check for a definition or a builtin at the current rewrite head and ingest it if there's one there
--   2) We try to apply our rewrite rules at the current rewrite head
--   3) We move onto the next element in the tree in DFS order. If we're at the end, we loop back to the start
-- Gives back (the amount of rules applied, whether we jumped back to the top of the tree). 
runStep :: RuntimeTV IO (Int, Bool)
runStep = do
    verbose <- gets runtimeVerbose
    let printZipper n = when verbose $ do
            zipper <- gets runtimeZipper
            lift $ putStr (show n ++ ": ")
            lift $ print zipper
    when verbose (lift $ putStrLn "== STARTING STEP ==")

    -- Begin 1
    printZipper 1 
    eatDef 
    printZipper 1.5
    eatBuiltin

    -- Begin 2
    printZipper 2
    onceDefs <- gets runtimeSingleUseRules -- >>= filterByMultiset
    repeatDefs <- gets runtimeRules -- >>= filterByMultiset
    -- Grab the first single use rule that satisfies all conditions
    tryDefinitions onceDefs 
    -- Apply single use tree rewriting rules
    maybeTreeRewrite <- applyTreeDefs onceDefs
    when (isJust maybeTreeRewrite) $ do -- A single use rule matched once, we gotta delete it!
        let (Rewrite patternToDelete _) = fromJust maybeTreeRewrite
        modifyRuntimeSingleUseRules (filter (patternDefEq patternToDelete))
    -- Apply multi use tree rewriting rules
    maybeTreeRewrite' <- applyTreeDefs repeatDefs
    -- Apply single use multiset only rules
    appliedMultisetDef <- applyMultisetDefs onceDefs
    when (isJust appliedMultisetDef) $ modifyRuntimeSingleUseRules (filter (/= fromJust appliedMultisetDef))
    -- Apply multi use multiset only rules
    appliedMultisetDef' <- applyMultisetDefs repeatDefs

    -- Begin 3 (I Love Laziness)
    printZipper 3
    newZipper <- gets (Z.nextDfs . runtimeZipper)
    modifyRuntimeZipper (const newZipper)
    
    -- Give back the value we use to assess termination
    printZipper 4
    atTop <- gets ((== []) . Z._Ups . runtimeZipper)
    let rulesApplied = sum (bool 0 1 <$> [isJust maybeTreeRewrite, isJust maybeTreeRewrite', isJust appliedMultisetDef, isJust appliedMultisetDef'])
    when verbose . lift . putStrLn . concat $ ["Rules applied: ", show rulesApplied, "; at top? ", show atTop]
    pure (rulesApplied, atTop)
        where
            -- True if the EatenDef has the same pattern as the tree
            patternDefEq :: Tree RValue -> EatenDef -> Bool
            patternDefEq pat1 def = case defToRewrite def of
                Nothing -> True
                Just (Rewrite pat2 _) -> pat1 /= pat2 
            tryBindConditions :: [MatchCondition] -> Runtime -> Binder_ Bool -- False if a condition didn't apply
            tryBindConditions [] _ = pure True
            tryBindConditions (cond:xs) r = do
                success <- applyMatchCondition cond r
                go <- tryBindConditions xs r
                pure $ success && go
            tryDefinitions :: [EatenDef] -> Runtime -> Binder_ (Maybe EatenDef)
            tryDefinitions defs r = let 
                bindActions = [(d, (`tryBindConditions` r) . defMatchCondition $ d) | d <- defs]
                binded = second (`runState` emptyBinder) <$> bindActions
                success = find (fst.snd) binded
                in pure (fst <$> success)



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
    pure (unzipper . runtimeZipper $ out, runtimeRules out)
    where
        -- we add one layer of `Branch` in Z.zipperFromTrees, let's pop it off here
        unzipper z = case Z.treeFromZipper z of
            Branch trees -> trees
            val@(Leaf _) -> [val]