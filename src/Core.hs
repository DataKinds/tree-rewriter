{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Core where

import qualified Data.Text as T
import Data.Maybe
import Data.List ( intercalate )
import qualified Data.Map as M
import Control.Monad.Trans.State.Lazy ( State, modify, runState )
import Control.Monad (zipWithM)
import Language.Haskell.TH.Syntax

-- Rewritable / runtime values
data RValue = RSymbol T.Text | RString T.Text | RNumber Integer deriving (Lift)

instance Show RValue where
    show (RSymbol t) = T.unpack t
    show (RString t) = T.unpack . T.concat $ ["\"", t, "\""]
    show (RNumber t) = show t

data Tree a = Branch [Tree a] | Leaf a deriving (Lift, Functor, Foldable, Traversable)

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

-- Tree rewrite rule datatype
-- Parameterized on leaf type
data Rewrite a = Rewrite (Tree (Pattern a)) (Tree (Pattern a)) deriving (Lift)

unpattern :: Tree (Pattern a) -> Maybe (Tree a)
unpattern = traverse f
    where
        f (PVariable _) = Nothing
        f (PExact a) = Just a

instance Show a => Show (Rewrite a) where
    show (Rewrite pattern template) = sexprprint pattern ++ " -to-> " ++ sexprprint template

-- ‧͙⁺˚*･༓☾ Try to match a single pattern at the tip of a tree ☽༓･*˚⁺‧͙ --
-- Statefully return the variables bound on a successful application --
--          Input tree     Pattern to match         Updated variable bindings, along with whether the match succeeded                        
tryApply :: Tree RValue -> Tree (Pattern RValue) -> State (M.Map T.Text (Tree RValue)) Bool
-- Bind pattern variables
tryApply rval (Leaf (PVariable pvar)) = modify (M.insert pvar rval) >> pure True
-- Match ntree branch patterns exactly
tryApply (Branch []) (Branch []) = pure True
tryApply (Branch []) _ = pure False
tryApply (Branch rtrees) (Branch pvals)
    -- No point to checking patterns that match branches of the wrong length
    | length rtrees /= length pvals = pure False
    | otherwise = and <$> zipWithM tryApply rtrees pvals
-- Match symbol patterns
tryApply (Leaf (RSymbol rsym)) (Leaf (PExact (RSymbol psym)))
    | rsym == psym = pure True
    | otherwise = pure False
-- Match number patterns
tryApply (Leaf (RNumber rnum)) (Leaf (PExact (RNumber pnum)))
    | rnum == pnum = pure True
    | otherwise = pure False
-- Match string patterns (TODO: use regex)
tryApply (Leaf (RString rstr)) (Leaf (PExact (RString pstr)))
    | rstr == pstr = pure True
    | otherwise = pure False
-- Catch failed matches
tryApply _ _ = pure False
    

-- DFS the input tree to try applying a rewrite rule anywhere it's possible --
-- Evaluates to the new tree and the number of rewrite rules applied in this step -- 
apply :: Tree RValue -> Rewrite RValue -> (Tree RValue, Integer)
apply rval rr@(Rewrite pval template) = let
    (success, bindings) = runState (tryApply rval pval) mempty
    in templateOrRecurse success bindings
    where
        -- We got a successful branch pattern match, let's rewrite to the template
        templateOrRecurse True bindings = (betaReduce bindings template, 1)
        templateOrRecurse False _ = case rval of
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
