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
    ( emptyBinder,
      RValue(RString),
      Tree(..), Binder_, runBinder )
import Core.DSL ( str, num, branch, tstr )
import qualified Zipper as Z
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (gets, execStateT, mapStateT)
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import qualified Multiset as MS
import Recognizers (recognizeDef, recognizeBuiltin, BuiltinRule (..))
import System.FilePath ((</>), takeDirectory)
import Parser (parse)
import Data.Bool (bool)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Multiset (cleanUp)
import Optics.State
import RuntimeEffects
import Data.Functor (void)
import Data.Foldable (for_)

emptyRuntime :: String -> Bool -> [Tree RValue] -> Runtime
emptyRuntime filepath verbose' trees = Runtime filepath verbose' emptyRules emptyRules (Z.zipperFromTrees trees) MS.empty 0 False
    where emptyRules = []

-- Execute a Rosin runtime --

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
                        Leaf (RString input) -> TIO.putStrLn input
                        other -> print other
                liftIO $ mapM_ printer args
                modifying #zipper (Z.nextDfs . Z.dropFocus)
            "parse" -> case args of
                Leaf (RString input):_ -> do
                    filepath <- gets runtimePath
                    case parse (T.unpack input) (filepath++"<eval>") of
                        Left err -> modifying #zipper (`Z.put` (Leaf . RString . T.pack $ "parse error: " ++ show err))
                        Right success -> modifying #zipper (`Z.spliceRight` success)
                _ -> pure ()
            "cat" -> case args of
                Leaf (RString path):_ -> do
                    pathContext <- gets (takeDirectory . runtimePath)
                    fileContents <- lift . TIO.readFile . (pathContext </>) . T.unpack $ path
                    modifying #zipper (`Z.put` (Leaf . RString $ fileContents))
                _ -> pure ()
            shouldntBePossible -> error$"Unrecognized builtin "++T.unpack shouldntBePossible++" that matched -- please report this as a bug!"

-- Runtime debug printing functions
printZipper :: String -> RuntimeM IO ()
printZipper l = gets runtimeVerbose >>= \v -> when v $ do
    zipper <- gets runtimeZipper
    lift $ putStr (l ++ ": ")
    lift $ print zipper
printLog :: String -> RuntimeM IO ()
printLog l = gets runtimeVerbose >>= \v -> when v . lift . putStrLn $ l

-- Try to apply our rewrite rules at the current rewrite head. Gives back the number of rules applied.
applyDefs :: RuntimeM IO Int
applyDefs = do
    onceDefs <- gets runtimeSingleUseRules
    repeatDefs <- gets runtimeRules
    printLog $ "Once defs: " ++ show onceDefs
    printLog $ "Repeat defs: " ++ show repeatDefs
    -- Grab the first single use rule that satisfies all conditions and apply it
    gets runtimeMultiset >>= \p -> printLog $ "Pocket: " ++ show p
    appliedOnceRule <- discardBinder $ applyRule onceDefs
    printLog $ "Once applied: " ++ show appliedOnceRule
    -- A single use rule matched once, we gotta delete it!
    for_ appliedOnceRule $ \rule -> modifying #singleUseRules (filter (/= rule))
    -- Grab the first multi use rule that satisfies all conditions and apply it
    appliedRule <- discardBinder $ applyRule repeatDefs
    printLog $ "Repeat applied: " ++ show appliedRule
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

-- Run `applyDefs`, `eatDef`, and `eatBuiltin` until there's no point...
fixEat :: RuntimeM IO Int
fixEat = do
    n <- eatBuiltin -- put, nextDfs . dropFocus, spliceRight
    n' <- fixApplyDefs
    n'' <- eatDef -- put, nextDfs . dropFocus
    n''' <- fixApplyDefs
    printZipper "Mid-fix"
    let count = n + n' + n'' + n'''
    if count == 0
        then pure 0
        else do
            tc <- fixApplyDefs
            pure $ count + tc

-- Carry out one step of Rosin's execution, then tail call if we are to continue execution.
-- Gives back the amount of rules applied in the final step (this should ALWAYS be 0!)
runStep :: RuntimeM IO Int
runStep = do
    verbose <- gets runtimeVerbose
    when verbose (lift $ putStrLn "== STARTING STEP ==")
    printZipper "Pre-step"

    -- Check for a definition or a builtin at the current rewrite head, ingest it if there's one there. Repeat this as many times as it will apply.
    count <- fixApplyDefs
    count' <- fixEat
    printZipper "Post-eat"

    -- Matching stuff usually moves our zipper forward, so we don't move if we matched on something.
    -- If we matched on anything, mark the execution as not finished.
    let rulesApplied = count + count'
    when (rulesApplied /= 0) $ assign #areWeDoneYet False
    when (rulesApplied == 0) $ modifying #zipper Z.nextDfs

    -- Let's stop executing if our "done" flag is set and we're back at the top of the input tree
    printZipper "Pre-termination"
    atTop <- gets ((== []) . Z._Ups . runtimeZipper)
    done <- gets runtimeAreWeDoneYet
    case (atTop, done) of
        (False, _) -> runStep
        (True, False) -> assign #areWeDoneYet True >> runStep
        (True, True) -> pure rulesApplied

-- | Sets up the Runtime to take control of its own execution through runStep.
firstStep :: RuntimeM IO ()
firstStep = assign #areWeDoneYet True >> assign #epoch 0 >> void runStep

-- | Executes a Rosin runtime
run :: Runtime -> IO Runtime
run = execStateT firstStep

-- we add one layer of `Branch` in Z.zipperFromTrees, let's pop it off here
unzipper z = case Z.treeFromZipper z of
    Branch trees -> trees
    val@(Leaf _) -> [val]

runEasy :: String -> Bool -> [Tree RValue] -> IO ([Tree RValue], [MatchRule])
runEasy filepath verbose inTrees = do
    out <- run (emptyRuntime filepath verbose inTrees)
    pure (unzipper . runtimeZipper $ out, runtimeRules out)
  