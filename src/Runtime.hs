module Runtime where
    
import Core
import Control.Monad.Trans.Accum
import qualified Data.Text as T
import Data.Foldable (foldrM)
import Debug.Trace
import Data.Either (rights)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity(Identity))


-- Runtime value eDSL -- 
-- Runtime leafs
rsym :: String -> Tree RValue
rsym = Leaf . RSymbol . T.pack
rstr :: String -> Tree RValue
rstr = Leaf . RString . T.pack
rnum :: Integer -> Tree RValue
rnum = Leaf . RNumber
-- Runtime branch
rbranch :: [Tree a] -> Tree a
rbranch = Branch

psym = rsym; pstr = rstr; pnum = rnum; pbranch = rbranch; 

-- Ruleset: Pattern and template building eDSL --
type WithRuleset = Accum Rules
type WithIORuleset = AccumT Rules IO
type Ruleset = WithRuleset ()

-- Rewriting rule
addRule :: Rewrite RValue -> Ruleset
addRule = add . Rules . pure

rule :: Tree RValue -> [Tree RValue] -> Ruleset
rule pattern templates = addRule $ Rewrite pattern templates

(~>) :: Tree RValue -> [Tree RValue] -> Ruleset
(~>) = rule

-- Pattern variables 
pvar :: String -> Tree RValue
pvar = Leaf . PVariable . T.pack

-- Running Ruleset --
-- Create a rules object from the Ruleset eDSL
makeRules :: WithRuleset a -> Rules 
makeRules = flip execAccum mempty

-- DFS a tree looking for definitions. Consume them and delete the tree branch containing the def.
eatDefs :: Tree RValue -> WithRuleset (Tree RValue)
eatDefs (Branch (pattern:(Leaf (RSymbol "~>")):templates)) = addRule (Rewrite pattern templates) >> pure (rbranch [rstr ruleStr])
    where
        ruleStr = sexprprint pattern ++ " ~> " ++ unwords (map sexprprint templates)
eatDefs (Branch bs) = Branch <$> mapM eatDefs bs
eatDefs l = pure l


liftIORuleset :: WithRuleset a -> WithIORuleset a
liftIORuleset = mapAccumT (\(Identity a) -> pure a)

-- Rewrite terms once, creating or modifying definitions as they arise
-- Left if no rewrite rules applied, right if some did
runStep :: [Tree RValue] -> WithIORuleset (Either [Tree RValue] [Tree RValue])
runStep [] = pure . pure $ []
runStep trees = do 
    -- detect definitions
    noDefsTrees <- liftIORuleset $ mapM eatDefs trees
    rules <- look
    -- apply rewrites
    let _lrTrees = flip applyRewrites rules <$> noDefsTrees 
    lrTrees <- lift $ sequence _lrTrees
    case rights lrTrees of
        -- no rewrites happened
        []  -> pure . Left $ noDefsTrees
        -- rewrites happened! be careful to not force their values here
        _ -> pure . Right $ concatMap (either id id) lrTrees

-- Runs a rewrite ruleset, from a starting ruleset, until it does not match
run :: Rules -> [Tree RValue] -> IO ([Tree RValue], Rules)
run rules inputTrees = runAccumT (add rules >> go inputTrees) mempty
    where
        go trees = do
            lrTree <- runStep trees
            case lrTree of
                Left trees' -> pure trees'
                Right trees' -> go trees'


