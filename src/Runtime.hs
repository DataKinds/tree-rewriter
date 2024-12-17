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
import Control.Monad.Trans.State (StateT (runStateT), modify, gets, execStateT)
import Control.Monad (unless, when)
import Data.Maybe (isJust, fromJust, catMaybes, mapMaybe)
import qualified Data.Map as M
import Data.Function (on)
import qualified Multiset as MS 
import Recognizers (EatenDef (..), UseCount (..), MultisetAction (..), recognizeDef, recognizeBuiltin, BuiltinRule (..))
import Data.Bifunctor (Bifunctor(bimap, second))
import System.FilePath (makeRelative, (</>), takeDirectory)
import Parser (parse)


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
type RuntimeTV m v = StateT Runtime m v
type RuntimeT m = RuntimeTV m ()

-- Runtime lenses -- 
updateRuntimeRules :: ([EatenDef] -> [EatenDef]) -> Runtime -> Runtime
updateRuntimeRules f b = b { runtimeRules = f (runtimeRules b) }
updateRuntimeSingleUseRules :: ([EatenDef] -> [EatenDef]) -> Runtime -> Runtime
updateRuntimeSingleUseRules f b = b { runtimeSingleUseRules = f (runtimeSingleUseRules b) }
updateRuntimeZipper :: (Z.Zipper RValue -> Z.Zipper RValue) -> Runtime -> Runtime
updateRuntimeZipper f b = b { runtimeZipper = f (runtimeZipper b) }

-- Runtime lens state modifiers --
modifyRuntimeRules :: Monad m => ([EatenDef] -> [EatenDef]) -> RuntimeT m
modifyRuntimeRules = modify . updateRuntimeRules
modifyRuntimeSingleUseRules :: Monad m => ([EatenDef] -> [EatenDef]) -> RuntimeT m
modifyRuntimeSingleUseRules = modify . updateRuntimeSingleUseRules
modifyRuntimeZipper :: Monad m => (Z.Zipper RValue -> Z.Zipper RValue) -> StateT Runtime m ()
modifyRuntimeZipper = modify . updateRuntimeZipper

-- Add a new tree rewriting rule into the runtime
addRule :: Monad m => EatenDef -> RuntimeT m
addRule rule = modifyRuntimeRules (rule:)

-- Add a new single use tree rewriting rule into the runtime
addSingleUseRule :: Monad m => EatenDef -> RuntimeT m
addSingleUseRule rule = modifyRuntimeSingleUseRules (rule:)


-- Parse and ingest definitions --

defUseCount :: EatenDef -> UseCount
defUseCount = \case 
    TreeDef uc _ _ -> uc
    MultisetDef uc _ -> uc
    ComboDef uc _ _ _ -> uc

defToRewrite :: EatenDef -> Maybe (Rewrite RValue)
defToRewrite (TreeDef _ pat templates) = Just $ Rewrite pat templates
defToRewrite (ComboDef _ _ pat templates) = Just $ Rewrite pat templates
defToRewrite _ = Nothing

-- What, if any, patterns does this definition want to match out of the multiset bag?
defWantsFromBag :: EatenDef -> MS.Multiset (Tree RValue)
defWantsFromBag (MultisetDef _ ma) = multisetPops ma
defWantsFromBag (ComboDef _ ma _ _) = multisetPops ma
defWantsFromBag _ = MS.empty

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

-- Check the current position of the rewrite head. If it's pointing to a builtin, rewrite it and execute any effects.
eatBuiltin :: RuntimeT IO
eatBuiltin = do
    subject <- gets (Z.look . runtimeZipper)
    case recognizeBuiltin subject of 
        Just (BuiltinRule name args) -> dispatch args name
        Nothing -> pure ()
    where 
        dispatch :: [Tree RValue] -> T.Text -> RuntimeT IO
        dispatch args = \case
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

-- Tries to apply all given rules at the focus of the runtime zipper, giving NO consideration to the multiset state.
-- Gives back a rule if it was applied. May mutate the zipper.
applyTreeDefs :: [EatenDef] -> RuntimeTV IO (Maybe (Rewrite RValue))
applyTreeDefs defs = do
    -- Apply regular tree rewriting rules
    subject <- gets (Z.look . runtimeZipper)
    (rewritten, maybeRewriteRule) <- lift $ apply subject (toTreeRules defs)
    case maybeRewriteRule of
        Nothing -> pure Nothing
        Just rewriteRule -> do
            modifyRuntimeZipper (`Z.spliceIn` rewritten)
            pure $ Just rewriteRule
    where toTreeRules :: [EatenDef] -> Rules
          toTreeRules = Rules . mapMaybe defToRewrite

-- Filters a list of defs down to only those which are satisfied by the current state of the multiset
filterByMultiset :: Monad m => [EatenDef] -> RuntimeTV m [EatenDef]
filterByMultiset defs = do
    pocket <- gets runtimeMultiset 
    pure $ filter (ok pocket) defs
    where 
        ok :: MS.Multiset (Tree RValue) -> EatenDef -> Bool
        ok ms def = MS.allInside (defWantsFromBag def) ms


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
    -- Apply single use tree rewriting rules
    defs <- gets runtimeSingleUseRules >>= filterByMultiset
    maybeTreeRewrite <- applyTreeDefs defs
    when (isJust maybeTreeRewrite) $ do -- A single use rule matched once, we gotta delete it!
        let (Rewrite patternToDelete _) = fromJust maybeTreeRewrite
        modifyRuntimeSingleUseRules (filter (patternDefEq patternToDelete))
    -- Apply multi use tree rewriting rules
    defs' <- gets runtimeRules >>= filterByMultiset
    maybeTreeRewrite' <- applyTreeDefs defs'

    -- Begin 3 (I Love Laziness)
    printZipper 3
    newZipper <- gets (Z.nextDfs . runtimeZipper)
    modifyRuntimeZipper (const newZipper)
    
    -- Give back the value we use to assess termination
    printZipper 4
    atTop <- gets ((== []) . Z._Ups . runtimeZipper)
    let rulesApplied = sum $ maybe 0 (const 1) <$> [maybeTreeRewrite, maybeTreeRewrite']
    when verbose (lift $ print rulesApplied)
    when verbose (lift $ print atTop)
    pure (rulesApplied, atTop)
        where
            -- True if the EatenDef has the same pattern as the tree
            patternDefEq :: Tree RValue -> EatenDef -> Bool
            patternDefEq pat1 def = case defToRewrite def of
                Nothing -> True
                Just (Rewrite pat2 _) -> pat1 /= pat2 


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