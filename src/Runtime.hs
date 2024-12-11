{-# LANGUAGE PatternSynonyms #-}

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
    runtimeMultiset :: M.Map (Tree RValue) Int
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
    multisetPops :: [Tree RValue],
    multisetPushs :: [Tree RValue]
} deriving (Show)

-- is this definition single use or will it apply forever?
data UseCount = UseOnce | UseMany deriving (Show)

-- What types of definition are there?
data EatenDef = TreeDef UseCount (Tree RValue) [Tree RValue]
              | MultisetDef UseCount MultisetAction
              | ComboDef UseCount MultisetAction (Tree RValue) [Tree RValue]
            deriving (Show)

defToRewrite :: EatenDef -> Maybe (Rewrite RValue)
defToRewrite (TreeDef _ pat templates) = Just $ Rewrite pat templates
defToRewrite (ComboDef _ _ pat templates) = Just $ Rewrite pat templates
defToRewrite _ = Nothing

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
    (pops:(LeafSym "|>"):[pushs]) -> pure . MultisetDef UseMany $ (MultisetAction `on` unbranch) pops pushs
    (pat:(LeafSym "~"):templates)-> pure $ TreeDef UseOnce pat templates
    (pops:(LeafSym "|"):[pushs]) -> pure . MultisetDef UseOnce $ (MultisetAction `on` unbranch) pops pushs
    (pops:(LeafSym "|"):pushs:(LeafSym "&"):pat:(LeafSym "~>"):templates) -> 
        pure $ ComboDef UseMany ((MultisetAction `on` unbranch) pops pushs) pat templates
    (pops:(LeafSym "|"):pushs:(LeafSym "&"):pat:(LeafSym ">"):templates) -> 
        pure $ ComboDef UseOnce ((MultisetAction `on` unbranch) pops pushs) pat templates
    _ -> Nothing

-- Check the current position of the rewrite head. If it's pointing to a definition, consume it.
eatDef :: Monad m => RuntimeT m
eatDef = do
    subject <- gets (Z.look . runtimeZipper)
    case recognizeDef subject of 
        -- handle regular old tree data rewrite rules
        Just td@(TreeDef UseMany pat templates) -> do
            addRule td
            let ruleStr = sexprprint pat ++ " ~> " ++ unwords (map sexprprint templates)
            modifyRuntimeZipper (flip Z.put $ branch [sym "defined", str ruleStr])
        Just td@(ComboDef UseMany (MultisetAction pops pushes) pat templates) -> do
            addRule td
            let ruleStr = concat [unwords $ map sexprprint pops, " | ", unwords $ map sexprprint pushes, " & ", sexprprint pat, " ~> ", unwords (map sexprprint templates)]
            modifyRuntimeZipper (flip Z.put $ branch [sym "defined", str ruleStr])
        -- handle one time use rewrite rules
        Just td@(TreeDef UseOnce pat templates) -> do
            addSingleUseRule td
            modifyRuntimeZipper Z.dropFocus
        _ -> pure ()

-- Execute a Rosin runtime --

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
    -- Apply regular tree rewriting rules
    rules <- gets (Rules . mapMaybe defToRewrite . runtimeRules)
    subject <- gets (Z.look . runtimeZipper)
    (rewritten, rewriteRule) <- lift $ apply subject rules 
    when (isJust rewriteRule) $ modifyRuntimeZipper (`Z.spliceIn` rewritten)
    -- Apply single use tree rewriting rules
    rules' <- gets (Rules . mapMaybe defToRewrite . runtimeSingleUseRules)
    subject' <- gets (Z.look . runtimeZipper)
    (rewritten', ruleToDelete) <- lift $ apply subject' rules' 
    when (isJust ruleToDelete) $ do
        modifyRuntimeZipper (`Z.spliceIn` rewritten')
        let (Rewrite patternToDelete _) = fromJust ruleToDelete
        modifyRuntimeSingleUseRules (filter (patternDefEq patternToDelete))
    -- Begin 3 (I Love Laziness)
    newZipper <- gets (Z.nextDfs . runtimeZipper)
    modifyRuntimeZipper (const newZipper)
    when verbose (lift $ print newZipper)
    -- Give back the value we use to assess termination
    atTop <- gets ((== []) . Z._Ups . runtimeZipper)
    pure (maybe 0 (const 1) rewriteRule, atTop)
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
emptyRuntime verbose trees = Runtime verbose emptyRules emptyRules (Z.zipperFromTrees trees) mempty
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