module DSL where
    
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
type Ruleset = Accum Rules ()

-- Rewriting rule
rule :: Tree (Pattern RValue) -> Tree (Pattern RValue) -> Ruleset
rule pattern template = add . Rules . pure $ Rewrite pattern template

-- Pattern leafs
pleaf :: a -> Tree (Pattern a)
pleaf = Leaf . PExact
psym :: String -> Tree (Pattern RValue)
psym = pleaf . RSymbol . T.pack
pstr :: String -> Tree (Pattern RValue)
pstr = pleaf . RString . T.pack
pnum :: Integer -> Tree (Pattern RValue)
pnum = pleaf . RNumber

-- Pattern branch
pbranch :: [Tree (Pattern a)] -> Tree (Pattern a)
pbranch = Branch

-- Pattern variable
pvar :: String -> Tree (Pattern a) 
pvar = Leaf . PVariable . T.pack

-- Running Ruleset --
-- Create a rules object from the Ruleset eDSL
makeRules :: Ruleset -> Rules 
makeRules = flip execAccum mempty

run :: Rules -> Tree RValue -> Tree RValue
run (Rules rewrites) inputTree = fix inputTree rewrites

runRuleset :: Ruleset -> Tree RValue -> Tree RValue
runRuleset ruleset = run (makeRules ruleset)