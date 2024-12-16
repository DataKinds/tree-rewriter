{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Runtime where
    
import Core
import qualified Zipper as Z
import qualified Data.Text as T
import Control.Monad.Trans.Class (lift)
import Data.Text.ICU (ParseError, regex')
import Control.Monad.Trans.State (StateT (runStateT), modify, gets, execStateT)
import Control.Monad (unless, when)
import Data.Maybe (isJust, fromJust, catMaybes, mapMaybe)
import qualified Data.Map as M
import Data.Function (on)
import qualified Multiset as MS 


-- Runtime value eDSL -- 
-- Runtime leaf values
sym :: String -> Tree RValue
sym = Leaf . RSymbol . T.pack
str :: String -> Tree RValue
str = Leaf . RString . T.pack
num :: Integer -> Tree RValue
num = Leaf . RNumber
regex :: String -> Either ParseError (Tree RValue)
regex s = regex' [] (T.pack s) >>= (pure . Leaf . RRegex)
-- Pattern variables 
pvar :: PVar -> Tree RValue
pvar = Leaf . RVariable 
-- Runtime branch
branch :: [Tree a] -> Tree a
branch = Branch

-- Runtime handles the state of the rewrite head processing the input data 
data Runtime = Runtime {
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

-- What action does matching this rewrite rule require on the multiset? 
data MultisetAction = MultisetAction {
    multisetPops :: MS.Multiset (Tree RValue),
    multisetPushs :: MS.Multiset (Tree RValue)
}

instance Show MultisetAction where
    show :: MultisetAction -> String
    show (MultisetAction pops pushs) = let 
        strPops = if MS.null pops then "" else show pops
        strPushs = if MS.null pushs then "" else show pushs
        in concat [strPops, "|", strPushs]

-- is this definition single use or will it apply forever?
data UseCount = UseOnce | UseMany deriving (Show, Eq)

-- What types of definition are there?
data EatenDef = TreeDef UseCount (Tree RValue) [Tree RValue]
              | MultisetDef UseCount MultisetAction
              | ComboDef UseCount MultisetAction (Tree RValue) [Tree RValue]

defUseCount :: EatenDef -> UseCount
defUseCount = \case 
    TreeDef uc _ _ -> uc
    MultisetDef uc _ -> uc
    ComboDef uc _ _ _ -> uc

instance Show EatenDef where
    show (TreeDef useCount pat templates) = let 
        op = if useCount == UseOnce then " ~ " else " ~> "
        in concat [sexprprint pat, op, unwords $ sexprprint <$> templates]
    show (MultisetDef useCount multisetAction) = show multisetAction -- TODO: shows UseMany rules wrong
    show (ComboDef useCount multisetAction pat templates) = let 
        op = if useCount == UseOnce then " ~ " else " ~> "
        in concat [show multisetAction, " & ", sexprprint pat, op, unwords $ sexprprint <$> templates]

defToRewrite :: EatenDef -> Maybe (Rewrite RValue)
defToRewrite (TreeDef _ pat templates) = Just $ Rewrite pat templates
defToRewrite (ComboDef _ _ pat templates) = Just $ Rewrite pat templates
defToRewrite _ = Nothing

-- What, if any, patterns does this definition want to match out of the multiset bag?
defWantsFromBag :: EatenDef -> MS.Multiset (Tree RValue)
defWantsFromBag (MultisetDef _ ma) = multisetPops ma
defWantsFromBag (ComboDef _ ma _ _) = multisetPops ma
defWantsFromBag _ = MS.empty

unbranch :: Tree RValue -> [Tree RValue]
unbranch (Branch trees) = trees
unbranch leaf = [leaf]

pattern LeafSym :: T.Text -> Tree RValue
pattern LeafSym sym = Leaf (RSymbol sym)

-- Given a tree, is the head of it listing out a rewrite rule?
recognizeDef :: Tree RValue -> Maybe EatenDef
recognizeDef (Leaf _) = Nothing --
recognizeDef (Branch trees) = case trees of
    (pat:(LeafSym "~>"):templates) -> pure $ TreeDef UseMany pat templates
    (pops:(LeafSym "|>"):[pushs]) -> pure . MultisetDef UseMany $ maFromTrees pops pushs
    (pat:(LeafSym "~"):templates)-> pure $ TreeDef UseOnce pat templates
    (pops:(LeafSym "|"):[pushs]) -> pure . MultisetDef UseOnce $ maFromTrees pops pushs
    (pops:(LeafSym "|"):pushs:(LeafSym "&"):pat:(LeafSym "~>"):templates) -> 
        pure $ ComboDef UseMany (maFromTrees pops pushs) pat templates
    (pops:(LeafSym "|"):pushs:(LeafSym "&"):pat:(LeafSym ">"):templates) -> 
        pure $ ComboDef UseOnce (maFromTrees pops pushs) pat templates
    _ -> Nothing
    where
        multisetFromTree :: Tree RValue -> MS.Multiset (Tree RValue)
        multisetFromTree = MS.fromList . map (,1) . unbranch
        maFromTrees :: Tree RValue -> Tree RValue -> MultisetAction
        maFromTrees = MultisetAction `on` multisetFromTree

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

-- Execute a Rosin runtime --

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
--   1) We check for a definition at the current rewrite head and ingest it if there's one there
--   2) We try to apply our rewrite rules at the current rewrite head
--   3) We move onto the next element in the tree in DFS order. If we're at the end, we loop back to the start
-- Gives back (the amount of rules applied, whether we jumped back to the top of the tree). 
runStep :: RuntimeTV IO (Int, Bool)
runStep = do
    verbose <- gets runtimeVerbose
    -- Begin 1
    eatDef 
    -- Begin 2
    -- Apply single use tree rewriting rules
    defs <- gets runtimeSingleUseRules
    filteredDefs <- filterByMultiset defs
    maybeTreeRewrite <- applyTreeDefs filteredDefs
    when (isJust maybeTreeRewrite) $ do -- A single use rule matched once, we gotta delete it!
        let (Rewrite patternToDelete _) = fromJust maybeTreeRewrite
        modifyRuntimeSingleUseRules (filter (patternDefEq patternToDelete))
    -- Apply multi use tree rewriting rules
    defs' <- gets runtimeRules
    filteredDefs' <- filterByMultiset defs'
    maybeTreeRewrite' <- applyTreeDefs filteredDefs'
    -- Begin 3 (I Love Laziness)
    newZipper <- gets (Z.nextDfs . runtimeZipper)
    modifyRuntimeZipper (const newZipper)
    when verbose (lift $ print newZipper)
    -- Give back the value we use to assess termination
    atTop <- gets ((== []) . Z._Ups . runtimeZipper)
    pure (sum $ maybe 0 (const 1) <$> [maybeTreeRewrite, maybeTreeRewrite'], atTop)
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

emptyRuntime :: Bool -> [Tree RValue] -> Runtime 
emptyRuntime verbose trees = Runtime verbose emptyRules emptyRules (Z.zipperFromTrees trees) MS.empty
    where emptyRules = []

runEasy :: Bool -> [Tree RValue] -> IO ([Tree RValue], [EatenDef])
runEasy verbose inTrees = do
    out <- run (emptyRuntime verbose inTrees)
    pure (unzipper . runtimeZipper $ out, runtimeRules out)
    where
        -- we add one layer of `Branch` in Z.zipperFromTrees, let's pop it off here
        unzipper z = case Z.treeFromZipper z of
            Branch trees -> trees
            val@(Leaf _) -> [val]