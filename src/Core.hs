module Core where

import qualified Data.Text as T
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.Trans.Except

-- Rewritable values
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


-- Nothing if no rewrites applied
applyRewrites :: Tree RValue -> [Rewrite RValue] -> Maybe (Tree RValue)
applyRewrites tree rewrites = listToMaybe . catMaybes $ map (apply tree) rewrites
    where
        betaReduce :: Tree (Pattern RValue) -> Map T.Text RValue -> Except String (Tree RValue)
        betaReduce (Branch trees) bindings = pure 

        apply :: Tree RValue -> Rewrite RValue -> Maybe (Tree RValue)
        apply (Branch trees) (Rewrite (Branch trees') template)
            | length trees == length trees' = 
                fmap Branch . 
                sequence . 
                map (uncurry apply) . 
                zip trees . 
                fmap (\tree -> Rewrite tree template) $ trees'
            | otherwise = Nothing
        apply _ (Rewrite (Branch trees') template) = Nothing
        apply (Leaf (RSymbol sym)) (Rewrite (RSymbol sym') template)
            | sym == sym' = Just 
            

fix :: Tree RValue -> [Rewrite RValue] -> Tree RValue
fix tree rewrites = case applyRewrites tree rewrites of 
    Just tree' -> fix tree' rewrites
    Nothing -> tree
