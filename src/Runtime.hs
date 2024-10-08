module Runtime where
    
import Core
import Control.Monad.Trans.Accum
import qualified Data.Text as T

-- Rule list used for execution
newtype Rules = Rules [Rewrite RValue]

unrules :: Rules -> [Rewrite RValue]
unrules (Rules rewrites) = rewrites

instance Show Rules where
    show (Rules rewrites) = unlines . concat $ [
        ["Rewrite rules:"],
        ["--------------"],
        show <$> rewrites]

instance Semigroup Rules where
    Rules rewrites <> Rules rewrites' = Rules (rewrites ++ rewrites')

instance Monoid Rules where
    mempty = Rules []

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

-- Ruleset: Pattern and template building eDSL --
type WithRuleset = Accum Rules 
type Ruleset = WithRuleset ()

-- Rewriting rule
addRule :: Rewrite RValue -> Ruleset
addRule = add . Rules . pure

rule :: Tree RValue -> [Tree RValue] -> Ruleset
rule pattern templates = addRule $ Rewrite pattern templates

(~>) :: Tree RValue -> [Tree RValue] -> Ruleset
(~>) = rule

-- Pattern leafs
pleaf :: a -> Tree a
pleaf = Leaf
psym :: String -> Tree RValue
psym = pleaf . RSymbol . T.pack
pstr :: String -> Tree RValue
pstr = pleaf . RString . T.pack
pnum :: Integer -> Tree RValue
pnum = pleaf . RNumber

-- Pattern branch
pbranch :: [Tree a] -> Tree a
pbranch = Branch

-- Pattern variables 
pvar :: String -> Tree RValue
pvar = Leaf . PVariable . T.pack

-- Running Ruleset --
-- Create a rules object from the Ruleset eDSL
makeRules :: WithRuleset a -> Rules 
makeRules = flip execAccum mempty

run :: Rules -> Tree RValue -> [Tree RValue]
run (Rules rewrites) inputTree = fix inputTree rewrites

-- DFS a tree looking for definitions
-- TODO
eatDefs :: Tree RValue -> WithRuleset (Tree RValue)
eatDefs (Branch [pattern, Leaf (RSymbol "~>"), template]) = addRule (Rewrite pattern template) >> (pure $ pbranch [])
eatDefs (Branch b:bs) = eatDefs b
        


-- Rewrite terms once, creating or modifying definitions as they arise
runStep :: [Tree RValue] -> WithRuleset [Tree RValue]
runStep [] = pure []
runStep (tree:trees) = do 
    (Rules rewrites) <- look
    -- detect definitions
    tree' <- eatDefs tree
    rest <- runStep trees
    return $ case applyRewrites tree' rewrites of 
        Just tree'' -> tree''++rest
        Nothing -> tree:rest
    -- mapM (`fix` rewrites)

-- runRuleset :: Ruleset -> Tree RValue -> [Tree RValue]
-- runRuleset ruleset = run (makeRules ruleset)