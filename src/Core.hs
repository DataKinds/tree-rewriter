module Core where

import qualified Data.Text as T
import Data.Maybe
import qualified Data.Map as M
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Debug.Trace

-- Rewritable / runtime values
data RValue = RSymbol T.Text | RString T.Text | RNumber Integer

instance Show RValue where
    show (RSymbol t) = T.unpack t
    show (RString t) = T.unpack . T.concat $ ["\"", t, "\""]
    show (RNumber t) = show t

data Tree a = Branch [Tree a] | Leaf a

prettyprint :: Show a => Tree a -> String
prettyprint t = pp 0 t 
    where  
        pp 0 (Leaf a) = show a
        pp d (Leaf a) = replicate d ' ' ++ "- " ++ show a
        pp d (Branch as) = unlines $ map (pp $ d+1) as

sexprprint :: Show a => Tree a -> String
sexprprint (Leaf a) = show a
sexprprint (Branch as) = "(" ++ unwords (map sexprprint as) ++ ")"

instance (Show a) => Show (Tree a) where
    show = prettyprint

-- Matchable patterns with holes for variables
data Pattern a = PExact a | PVariable T.Text 

instance Show a => Show (Pattern a) where
    show (PExact a) = show a
    show (PVariable t) = ':':T.unpack t

data Rewrite a = Rewrite (Tree (Pattern a)) (Tree (Pattern a))

instance Show a => Show (Rewrite a) where
    show (Rewrite pattern template) = sexprprint pattern ++ " -> " ++ sexprprint template


-- Apply a single rewrite rule --
apply :: Tree RValue -> Rewrite RValue -> StateT (M.Map T.Text (Tree RValue)) Maybe (Tree RValue)
-- Bind pattern variables
apply rval (Rewrite (Leaf (PVariable pvar)) _) = do
    modify (M.insert pvar rval)
    lift . pure $ Branch []
-- Recurse into runtime value (DFS) if there's no match
apply (Branch rtrees) rewrite@(Rewrite (Leaf pval) template) = do
    bindings <- get
    let rewrittenChildren :: [Maybe (Tree RValue)]
        rewrittenChildren = map (flip evalStateT bindings) $ map (\rt -> apply rt rewrite) rtrees
        conditionalUpdate og (Just rewritten) = rewritten
        conditionalUpdate og Nothing = og
    if all isNothing rewrittenChildren then
        lift Nothing
    else  
        lift . pure . Branch $ zipWith conditionalUpdate rtrees rewrittenChildren
-- Recurse into pattern trees
apply (Branch rtrees) (Rewrite (Branch ptrees) template)
    | length rtrees == length ptrees = do
        bindings <- get
        let subptrees = (\t -> Rewrite t (Branch [])) <$> ptrees
        _ <- sequence $ zipWith apply rtrees subptrees
        lift . pure . betaReduce bindings $ template 
    | otherwise = lift Nothing
apply _ (Rewrite (Branch _) _) = lift Nothing
-- Match symbol patterns
apply (Leaf (RSymbol rsym)) (Rewrite (Leaf (PExact (RSymbol psym))) template)
    | rsym == psym = get >>= \bindings -> lift . Just . betaReduce bindings $ template
    | otherwise = lift Nothing
apply _ (Rewrite (Leaf (PExact (RSymbol _))) _) = lift Nothing
-- Match number patterns
apply (Leaf (RNumber rnum)) (Rewrite (Leaf (PExact (RNumber pnum))) template)
    | rnum == pnum = get >>= \bindings -> lift . Just . betaReduce bindings $ template
    | otherwise = lift Nothing
apply _ (Rewrite (Leaf (PExact (RNumber _))) _) = lift Nothing
-- TODO: Match string patterns using regex
apply (Leaf (RString rstr)) (Rewrite (Leaf (PExact (RString pstr))) template)
    | rstr == pstr = get >>= \bindings -> lift . Just . betaReduce bindings $ template
    | otherwise = lift Nothing
apply _ (Rewrite (Leaf (PExact (RString _))) _) = lift Nothing

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
applyRewrites tree rewrites = listToMaybe . catMaybes . map (flip evalStateT mempty) $ map (apply tree) rewrites


fix :: Tree RValue -> [Rewrite RValue] -> Tree RValue
fix tree rewrites = case applyRewrites tree rewrites of 
    Just tree' -> trace (show tree') $ fix tree' rewrites
    Nothing -> tree
