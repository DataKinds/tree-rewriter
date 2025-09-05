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
import Core.DSL ( str, num, branch, tstr )
import qualified Zipper as Z
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (gets, execStateT, mapStateT, get)
import Control.Monad (when, ap)
import Data.Maybe (isJust, fromJust)
import qualified Multiset as MS
import Recognizers 
import System.FilePath ((</>), takeDirectory)
import Parser (parse)
import Data.Bool (bool)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Multiset (cleanUp)
import Optics.State
import RuntimeEffects
import Data.Functor (void)
import Data.Foldable (for_)
import Control.Arrow ((&&&))
import Prettyprinter.Render.Terminal (putDoc, color, Color (Red))

emptyRuntime :: String -> Bool -> [Tree RValue] -> Runtime
emptyRuntime filepath verbose' trees = Runtime filepath verbose' emptyRules emptyRules (Z.zipperFromTrees (epoch-1) trees) MS.empty epoch True 0
    where emptyRules = []
          epoch = 0

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
        modifying #zipper (Z.updateFocus $ tagAll epoch) 
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
  