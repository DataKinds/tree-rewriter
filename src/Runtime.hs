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
      emptyBinder,
      rebranch,
      tryApply,
      Binder_,
      RValue(RString),
      Tree(..) )
import Core.DSL ( sym, str, num, branch, pvar, regex, tstr )
import qualified Zipper as Z
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (runStateT), gets, execStateT, State, runState, state, get, put)
import Control.Monad (unless, when)
import Data.Maybe (isJust, fromJust)
import qualified Multiset as MS
import Recognizers (recognizeDef, recognizeBuiltin, BuiltinRule (..))
import Data.Bifunctor (Bifunctor(second))
import System.FilePath ((</>), takeDirectory)
import Parser (parse)
import Data.Bool (bool)
import RuntimeEffects (MatchCondition (..), MatchRule, UseCount (..), MatchEffect (..), useCount, matchCondition, matchEffect)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (find)
import Multiset (cleanUp)
import Data.Semigroup (Semigroup(sconcat), Any (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Functor.Identity (Identity(..))
import Optics.State
import Optics
import RuntimeEffects


emptyRuntime :: String -> Bool -> [Tree RValue] -> Runtime
emptyRuntime filepath verbose' trees = Runtime filepath verbose' emptyRules emptyRules (Z.zipperFromTrees trees) MS.empty
    where emptyRules = []

-- Execute a Rosin runtime --

-- | Add a new tree rewriting rule into the runtime
addRule :: Monad m => MatchRule -> RuntimeT m
addRule rule = case useCount rule of
    UseOnce -> modifying #singleUseRules (rule:)
    UseMany -> modifying #rules (rule:)

-- | Check the current position of the rewrite head. If it's pointing to a definition, consume it.
-- Gives back the count of definitions consumed.
eatDef :: Monad m => RuntimeTV m Int
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
eatBuiltin :: RuntimeTV IO Int
eatBuiltin = do
    subject <- gets (Z.look . runtimeZipper)
    case recognizeBuiltin subject of
        Just (BuiltinRule name args) -> do 
            dispatchBuiltin args name
            pure 1
        Nothing -> pure 0
    where
        -- | Execute a given builtin. The `args` passed in are the args passed to the builtin.
        dispatchBuiltin :: [Tree RValue] -> T.Text -> RuntimeT IO
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
printZipper :: String -> RuntimeT IO
printZipper l = gets runtimeVerbose >>= \v -> when v $ do
    zipper <- gets runtimeZipper
    lift $ putStr (l ++ ": ")
    lift $ print zipper
printLog :: String -> RuntimeT IO
printLog l = gets runtimeVerbose >>= \v -> when v . lift . putStrLn $ l

-- Try to apply our rewrite rules at the current rewrite head. Gives back the number of rules applied.
applyDefs :: RuntimeTV IO Int
applyDefs = do
    onceDefs <- gets runtimeSingleUseRules
    repeatDefs <- gets runtimeRules
    printLog $ "Once defs: " ++ show onceDefs
    printLog $ "Repeat defs: " ++ show repeatDefs
    -- Grab the first single use rule that satisfies all conditions and apply it
    gets runtimeMultiset >>= \p -> printLog $ "Pocket: " ++ show p
    appliedOnceRule <- applyFirstMatchingDefinition onceDefs
    printLog $ "Once applied: " ++ show appliedOnceRule
    -- A single use rule matched once, we gotta delete it!
    when (isJust appliedOnceRule) $ modifying #singleUseRules (filter (/= fromJust appliedOnceRule))
    -- Grab the first multi use rule that satisfies all conditions and apply it
    appliedRule <- applyFirstMatchingDefinition repeatDefs
    printLog $ "Repeat applied: " ++ show appliedRule
    pure $ sum (bool 0 1 . isJust <$> [appliedOnceRule, appliedRule])
        where
            -- Fully apply the first matching definition we can find. Mutate the runtime, give back Nothing if we can't
            applyFirstMatchingDefinition :: [MatchRule] -> RuntimeTV IO (Maybe MatchRule)
            applyFirstMatchingDefinition defs = do
                runtime <- get
                let (matchedDef, binder) = tryDefinitions defs runtime `runState` emptyBinder
                case matchedDef of
                    Just def -> let
                            matchAction = mapM_ applyMatchEffect . matchEffect $ def
                            bang = (`runStateT` binder) $ runStateT matchAction runtime -- TODO: brother...
                            ((_, runtime'), _) = runIdentity bang
                        in do
                            put runtime'
                            pure $ Just def
                    Nothing -> pure Nothing

-- Run `applyDefs` until there's no point...
fixApplyDefs :: RuntimeTV IO Int
fixApplyDefs = do
    n <- applyDefs
    if n == 0
        then pure 0
        else do
            modifying #multiset cleanUp
            tc <- fixApplyDefs
            pure $ n + tc

-- Run `applyDefs`, `eatDef`, and `eatBuiltin` until there's no point...
fixEat :: RuntimeTV IO Int
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

-- Carry out one step of Rosin's execution. This essentially carries out the following:
--   1) We check for a definition or a builtin at the current rewrite head and ingest it if there's one there
--   2) We try to apply our rewrite rules at the current rewrite head as many times as possible. Between steps we repeat 1, checking for defs and builtins.
--   3) We move onto the next element in the tree in DFS order. If we're at the end, we loop back to the start
-- Gives back (the amount of rules applied, whether we jumped back to the top of the tree). 
runStep :: RuntimeTV IO (Int, Bool)
runStep = do
    verbose <- gets runtimeVerbose
    when verbose (lift $ putStrLn "== STARTING STEP ==")
    printZipper "Pre-step"

    -- Begin 1 & 2
    count <- fixApplyDefs
    count' <- fixEat
    printZipper "Post-eat"

    -- Begin 3 if we never matched anything. Matching stuff usually moves our zipper forward, so we don't move if we matched
    let rulesApplied = count + count'
    when (rulesApplied == 0) $ modifying #zipper Z.nextDfs

    -- Give back the value we use to assess termination
    printZipper "Pre-termination"
    atTop <- gets ((== []) . Z._Ups . runtimeZipper)
    when verbose . lift . putStrLn . concat $ ["Rules applied: ", show rulesApplied, "; at top? ", show atTop]
    pure (rulesApplied, atTop)


-- Run `runStep` until there's no point...
fixStep :: RuntimeT IO
fixStep = go 0
    where
        readyToStop = do
            (count, reset) <- runStep
            if count > 0 then
                go count
            else 
                unless reset readyToStop
        go count = do
            (count', reset) <- runStep
            if reset
                then readyToStop
                else go $ count + count'


-- Executes a Rosin runtime
run :: Runtime -> IO Runtime
run = execStateT fixStep

-- we add one layer of `Branch` in Z.zipperFromTrees, let's pop it off here
unzipper z = case Z.treeFromZipper z of
    Branch trees -> trees
    val@(Leaf _) -> [val]

runEasy :: String -> Bool -> [Tree RValue] -> IO ([Tree RValue], [MatchRule])
runEasy filepath verbose inTrees = do
    out <- run (emptyRuntime filepath verbose inTrees)
    pure (unzipper . runtimeZipper $ out, runtimeRules out)
  