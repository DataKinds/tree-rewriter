module Core where

import qualified Data.Text as T
import Data.Maybe
import Data.List ( intercalate )
import qualified Data.Map as M
import Control.Monad.Trans.State.Lazy
import Control.Monad (zipWithM)
import Language.Haskell.TH.Syntax

-- Rewritable / runtime values
data RValue = RSymbol T.Text | RString T.Text | RNumber Integer deriving (Lift)

instance Show RValue where
    show (RSymbol t) = T.unpack t
    show (RString t) = T.unpack . T.concat $ ["\"", t, "\""]
    show (RNumber t) = show t

data Tree a = Branch [Tree a] | Leaf a deriving (Lift)

prettyprint :: Show a => Tree a -> String
prettyprint = pp 0
    where  
        pp 0 (Leaf a) = show a
        pp d (Leaf a) = replicate d ' ' ++ "- " ++ show a
        pp d (Branch as) = intercalate "\n" $ map (pp $ d+1) as

sexprprint :: Show a => Tree a -> String
sexprprint (Leaf a) = show a
sexprprint (Branch as) = "(" ++ unwords (map sexprprint as) ++ ")"

instance (Show a) => Show (Tree a) where
    show = prettyprint

-- Matchable patterns with holes for variables
data Pattern a = PExact a | PVariable T.Text deriving (Lift)

instance Show a => Show (Pattern a) where
    show (PExact a) = show a
    show (PVariable t) = ':':T.unpack t

data Rewrite a = Rewrite (Tree (Pattern a)) (Tree (Pattern a))

instance Show a => Show (Rewrite a) where
    show (Rewrite pattern template) = sexprprint pattern ++ " -to-> " ++ sexprprint template

-- ‧͙⁺˚*･༓☾ Try to match a single pattern at the tip of a tree ☽༓･*˚⁺‧͙ --
-- Return the variables bound on a successful application --
getMatchSuccess (m, _, _) = m
getMatchLength (_, l, _) = l
getMatchOffset (_, _, o) = o

--          Starting match offset  Pattern to match         Updated variable bindings, along with 3 things: 
--                  Input tree                              whether the match succeeded, and the match length and offset for branches only
tryApply :: Int ->  Tree RValue -> Tree (Pattern RValue) -> State (M.Map T.Text (Tree RValue)) (Bool, Int, Int)
-- Bind pattern variables
tryApply _ rval (Leaf (PVariable pvar)) = do
    modify (M.insert pvar rval)
    pure (True, 1, 0)
-- Match branches, and try branch tails if we fail
tryApply o (Branch []) (Branch []) = pure (True, 0, o)
tryApply _ (Branch []) _ = pure (False, 0, 0)
tryApply o (Branch rtrees) pattern@(Branch pvals)
    | length rtrees > length pvals = do
        success <- all getMatchSuccess <$> zipWithM (tryApply o) rtrees pvals
        if success then pure (True, length pvals, o)
        -- We failed to match at the head of this branch, let's try its tail
        else tryApply (o+1) (Branch $ tail rtrees) pattern
    -- No point to checking the branch tail since we know the pattern length
    | length rtrees == length pvals = do
        success <- all getMatchSuccess <$> zipWithM (tryApply 0) rtrees pvals
        pure (success, length pvals, o)
    | otherwise = pure (False, 0, o)
-- Match symbol patterns
tryApply _ (Leaf (RSymbol rsym)) (Leaf (PExact (RSymbol psym)))
    | rsym == psym = pure (True, 1, 0)
    | otherwise = pure (False, 1, 0)
-- Match number patterns
tryApply _ (Leaf (RNumber rnum)) (Leaf (PExact (RNumber pnum)))
    | rnum == pnum = pure (True, 1, 0)
    | otherwise = pure (False, 1, 0)
-- Match string patterns (TODO: use regex)
tryApply _ (Leaf (RString rstr)) (Leaf (PExact (RString pstr)))
    | rstr == pstr = pure (True, 1, 0)
    | otherwise = pure (False, 1, 0)
-- Catch failed matches
tryApply _ _ _ = pure (False, 0, 0)
    

-- DFS the input tree to try applying a rewrite rule anywhere it's possible --
-- Evaluates to the new tree and the number of rewrite rules applied in this step -- 
apply :: Tree RValue -> Rewrite RValue -> (Tree RValue, Integer)
apply rval rr@(Rewrite pval template) = let
    ((success, matchLength, matchOffset), bindings) = runState (tryApply 0 rval pval) mempty
    in templateOrRecurse (success, matchLength, matchOffset) bindings
    where
        -- We got a successful pattern match, let's rewrite to the template
        templateOrRecurse (True, matchLength, matchOffset) bindings = case rval of
            -- We matched a branch, we must be careful to only rewrite based on matchLength and matchOffset
            Branch rtrees -> if matchLength == length rtrees
                then (betaReduce bindings template, 1) 
                else let
                    (preMatch, matchAndPostMatch) = splitAt matchOffset rtrees
                    (_, postMatch) = splitAt matchLength matchAndPostMatch
                    templatedLeaf = case betaReduce bindings template of
                        leaf@(Leaf _) -> [leaf]
                        (Branch bs) -> bs
                in (Branch $ preMatch ++ templatedLeaf ++ postMatch, 1)
            -- We matched a leaf, we can rewrite directly
            Leaf _ -> (betaReduce bindings template, 1)
        -- We did not get a successful pattern match, let's recurse
        templateOrRecurse (False, _, _) _ = case rval of
            -- If we failed to match a nub branch, we give it back unchanged
            Branch [] -> (rval, 0)
            -- If we failed to match an inhabited branch, let's match the branch's children
            Branch rtrees -> let
                (rtrees', count) = foldr (\(rtree, apCount) (accTree, accCount) -> (rtree:accTree, apCount+accCount)) ([], 0) (flip apply rr <$> rtrees)
                in (Branch rtrees', count)
            -- If we failed to match a single runtime value, we give it back unchanged
            Leaf _ -> (rval, 0)

-- Fill in variable bindings inside a pattern
-- TODO: this function can bottom, let's return errors with a proper monad!
betaReduce :: M.Map T.Text (Tree RValue) -> Tree (Pattern RValue) -> Tree RValue
betaReduce bindings (Branch trees) = Branch . map (betaReduce bindings) $ trees
betaReduce _ (Leaf (PExact pval)) = Leaf pval
betaReduce bindings (Leaf (PVariable pvar)) = case bindings M.!? pvar of
    Just rval -> rval
    Nothing -> error . T.unpack $ T.append "Missing binding for variable " pvar

-- Nothing if no rewrites applied
applyRewrites :: Tree RValue -> [Rewrite RValue] -> Maybe (Tree RValue)
applyRewrites rval = listToMaybe . mapMaybe maybeApply
    where
        maybeApply :: Rewrite RValue -> Maybe (Tree RValue)
        maybeApply rewrite = case apply rval rewrite of
            (_, 0) -> Nothing
            (rval', _) -> Just rval'


fix :: Tree RValue -> [Rewrite RValue] -> Tree RValue
fix tree rewrites = case applyRewrites tree rewrites of 
    Just tree' -> fix tree' rewrites
    Nothing -> tree
