module Runtime where
    
import Core
import qualified Zipper as Z
import Control.Monad.Trans.Accum
import qualified Data.Text as T
import Debug.Trace
import Data.Either (rights)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity(Identity))
import Data.Text.ICU (ParseError, regex')
import Control.Monad.Trans.State (StateT (runStateT), modify, gets, put, execStateT)
import Data.Maybe (catMaybes)
import Control.Monad (guard, unless)


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
    -- What tree rewriting rules are active?
    runtimeRules :: Rules,
    -- What tree rewriting lambdas are active?
    runtimeSingleUseRules :: Rules,
    -- Where are we in the data tree?
    runtimeZipper :: Z.Zipper RValue
}
type RuntimeTV m v = StateT Runtime m v
type RuntimeT m = RuntimeTV m ()

-- Runtime lenses -- 
updateRuntimeRules :: (Rules -> Rules) -> Runtime -> Runtime
updateRuntimeRules f b = b { runtimeRules = f (runtimeRules b) }
updateRuntimeSingleUseRules :: (Rules -> Rules) -> Runtime -> Runtime
updateRuntimeSingleUseRules f b = b { runtimeSingleUseRules = f (runtimeSingleUseRules b) }
updateRuntimeZipper :: (Z.Zipper RValue -> Z.Zipper RValue) -> Runtime -> Runtime
updateRuntimeZipper f b = b { runtimeZipper = f (runtimeZipper b) }

-- Runtime lens state modifiers --
modifyRuntimeRules :: Monad m => (Rules -> Rules) -> RuntimeT m
modifyRuntimeRules = modify . updateRuntimeRules
modifyRuntimeSingleUseRules :: Monad m => (Rules -> Rules) -> RuntimeT m
modifyRuntimeSingleUseRules = modify . updateRuntimeSingleUseRules
modifyRuntimeZipper :: Monad m => (Z.Zipper RValue -> Z.Zipper RValue) -> StateT Runtime m ()
modifyRuntimeZipper = modify . updateRuntimeZipper

-- Add a new tree rewriting rule into the runtime
addTreeRule :: Monad m => Rewrite RValue -> RuntimeT m
addTreeRule rule = modifyRuntimeRules (Rules . (rule:) . unrules)

-- Add a new single use tree rewriting rule into the runtime
addSingleUseTreeRule :: Monad m => Rewrite RValue -> RuntimeT m
addSingleUseTreeRule rule = modifyRuntimeSingleUseRules (Rules . (rule:) . unrules)


-- Parse and ingest definitions --

-- What types of definition are there?
data DefStyle = TreeDef (Tree RValue) [Tree RValue] 
              | TreeSingleUseDef (Tree RValue) [Tree RValue]

-- Given a tree, is the head of it listing out a rewrite rule?
recognizeDef :: Tree RValue -> Maybe DefStyle
recognizeDef (Branch (pattern:(Leaf (RSymbol "~>")):templates)) = pure $ TreeDef pattern templates
recognizeDef (Branch (pattern:(Leaf (RSymbol "\\")):templates)) = pure $ TreeSingleUseDef pattern templates
recognizeDef _ = Nothing

-- Check the current position of the rewrite head. If it's pointing to a definition, consume it.
eatDef :: Monad m => RuntimeT m
eatDef = do
    subject <- gets (Z.look . runtimeZipper)
    case recognizeDef subject of 
        -- handle regular old tree data rewrite rules
        Just (TreeDef pattern templates) -> do
            addTreeRule (Rewrite pattern templates)
            let ruleStr = sexprprint pattern ++ " ~> " ++ unwords (map sexprprint templates)
            modifyRuntimeZipper (flip Z.put $ branch [sym "defined", str ruleStr])
        -- handle one time use rewrite rules
        Just (TreeSingleUseDef pattern templates) -> do
            addSingleUseTreeRule (Rewrite pattern templates)
            modifyRuntimeZipper (flip Z.put $ branch []) -- TODO: I kinda want lambda rules to just be eaten entirely with no trace
        _ -> pure ()

-- Execute a Rosin runtime --

-- Carry out one step of Rosin's execution. This essentially carries out the following:
--   1) We check for a definition at the current rewrite head and ingest it if there's one there
--   2) We try to apply our rewrite rules at the current rewrite head
--   3) We move onto the next element in the tree in DFS order. If we're at the end, we loop back to the start
-- Gives back (the amount of rules applied, whether we jumped back to the top of the tree). There should never be more than 1 rule applied per step.
runStep :: RuntimeTV IO (Int, Bool)
runStep = do
    -- Begin 1
    eatDef 
    -- Begin 2
    rules <- gets runtimeRules 
    subject <- gets (Z.look . runtimeZipper)
    (rewritten, rewriteCount) <- lift $ apply subject rules 
    modifyRuntimeZipper (`Z.put` Branch rewritten) -- TODO: `Branch` is wrong here, right?
    -- Begin 3 (I Love Laziness)
    downLook <- gets (Z.firstChild . runtimeZipper)
    rightLook <- gets (Z.right . runtimeZipper)
    -- TODO: if rightLook fails, we need to go up + right. if thatfails we need to go up + up + right. if that fails... 
    startLook <- gets (Z.upmost . runtimeZipper)
    let newZipper = head (catMaybes [downLook, rightLook, Just startLook])
    modifyRuntimeZipper (const newZipper)
    -- Give back the value we use to assess termination
    atTop <- gets ((== []) . Z._Ups . runtimeZipper)
    pure (rewriteCount, atTop)

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

runEasy :: [Tree RValue] -> IO ([Tree RValue], Rules)
runEasy inTrees = do
    out <- run (Runtime emptyRules emptyRules (Z.zipperFromTrees inTrees))
    pure (unzipper . runtimeZipper $ out, runtimeRules out)
    where
        -- we add one layer of `Branch` in Z.zipperFromTrees, let's pop it off here
        unzipper z = case Z.treeFromZipper z of
            Branch trees -> trees
            val@(Leaf _) -> [val]
        emptyRules = Rules []