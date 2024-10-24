{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
module Core where

import qualified Data.Text as T
import Data.Maybe
import Data.List ( intercalate )
import qualified Data.Map as M
import Control.Monad.Trans.State.Lazy ( State, modify, runState )
import Control.Monad (zipWithM)
import Language.Haskell.TH.Syntax
import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import Debug.Trace (trace)

-- Runtime values, including pattern variables
data RValue = RSymbol T.Text | RString T.Text | RNumber Integer | PVariable T.Text deriving (Lift, Eq, Ord)

instance Show RValue where
    show (RSymbol t) = T.unpack t
    show (RString t) = T.unpack . T.concat $ ["\"", t, "\""]
    show (RNumber t) = '+':show t
    show (PVariable t) = ':':T.unpack t

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

-- Tree rewrite rule datatype
-- Parameterized on leaf type
data Rewrite a = Rewrite (Tree a) [Tree a] deriving (Lift)
rewritePattern :: Rewrite a -> Tree a
rewritePattern (Rewrite pat _) = pat
rewriteTemplate :: Rewrite a -> [Tree a]
rewriteTemplate (Rewrite _ templ) = templ


instance Show a => Show (Rewrite a) where
    show (Rewrite pattern templates) = sexprprint pattern ++ " -to-> " ++ unwords (sexprprint <$> templates)

-- ‧͙⁺˚*･༓☾ Try to match a single pattern at the tip of a tree ☽༓･*˚⁺‧͙ --
-- Statefully return the variables bound on a successful application --
--          Input tree     Pattern to match         Updated variable bindings, along with whether the match succeeded                        
tryApply :: Tree RValue -> Tree RValue -> State (M.Map T.Text [Tree RValue]) Bool
-- Bind pattern variables and special accumulators
tryApply rval (Leaf (PVariable pvar))
    -- Bind special accumulator
    | T.head pvar == '?' = case pvar of
        -- sum accumulator 
        "?+" -> case rval of
            num@(Leaf (RNumber _)) -> addBinding pvar num
            _ -> pure False
        -- product accumulator 
        "?*" -> case rval of
            num@(Leaf (RNumber _)) -> addBinding pvar num
            _ -> pure False
        -- negation accumulator 
        "?-" -> case rval of
            num@(Leaf (RNumber _)) -> addBinding pvar num
            _ -> pure False
        -- output accumulator
        "?>" -> addBinding pvar rval
        -- input accumulator
        "?<" -> error "Unimplemented input accumulator :?<"
        -- cons to sexpr (pack) accumulator 
        "?@" -> let
                deepCollect :: [Tree RValue] -> [Tree RValue]
                deepCollect [] = []
                deepCollect (val:(Leaf end):_) = [val, Leaf end]
                deepCollect (val:(Branch rest):_) = val : deepCollect rest
                deepCollect [end] = [end]
            in case rval of
                (Leaf _) -> pure False
                (Branch rs) -> addBinding pvar (Branch $ deepCollect rs)
        -- sexpr to cons (unpack) accumulator 
        "?%" -> error "Unimplemented input accumulator :?<"
        _ -> error . T.unpack $ T.append "Tried binding unknown special accumulator " pvar
    -- Bind eager variable (aka: a var that only binds with a term that cannot be rewritten)
    | T.head pvar == '!' = case rval of
        -- TODO: invoke searchPatterns to check for rewritability before matching
        (Leaf _) -> addBinding pvar rval
        (Branch _) -> pure False
    -- Bind regular pattern variable
    | otherwise = addBinding pvar rval
    where
        -- takes a name and a runtime value tree, creates a new binding or appends to a binding list
        addBinding name binding = modify (M.alter (pure . \case { Nothing -> [binding] ; (Just bindings) -> binding:bindings }) name) >> pure True
-- Match ntree branch patterns exactly
tryApply (Branch []) (Branch []) = pure True
tryApply (Branch []) _ = pure False
tryApply (Branch rtrees) (Branch pvals)
    -- No point to checking patterns that match branches of the wrong length
    | length rtrees /= length pvals = pure False
    | otherwise = and <$> zipWithM tryApply rtrees pvals
-- Match symbol patterns
tryApply (Leaf (RSymbol rsym)) (Leaf (RSymbol psym))
    | rsym == psym = pure True
    | otherwise = pure False
-- Match number patterns
tryApply (Leaf (RNumber rnum)) (Leaf (RNumber pnum))
    | rnum == pnum = pure True
    | otherwise = pure False
-- Match string patterns (TODO: use regex)
tryApply (Leaf (RString rstr)) (Leaf (RString pstr))
    | rstr == pstr = pure True
    | otherwise = pure False
-- Catch failed matches
tryApply _ _ = pure False

fst3 :: (a, b, c) -> a
fst3 (a,_,_)=a

-- DFS the input tree, attempting to apply all rewrite rules at every location
-- On a successful rule match, give back the pattern variable bindings and the template
searchPatterns :: Tree RValue -> Rules -> Maybe (M.Map T.Text [Tree RValue], [Tree RValue])
searchPatterns rval (Rules rules) = let
    patterns = rewritePattern <$> rules
    templates = rewriteTemplate <$> rules
    makeMatchAttempts = tryApply rval <$> patterns
    matchAttemptsWithTemplates = zipWith (\(success, binding) template -> (success,binding,template)) (flip runState mempty <$> makeMatchAttempts) templates
    (_, possibleSuccess) = break fst3 matchAttemptsWithTemplates
    in case possibleSuccess of 
        (_, binding, templ):_ -> Just (binding, templ)
        [] -> Nothing

-- DFS the input tree to try applying all rewrite rules anywhere it's possible --
-- Evaluates to the new tree and the number of rewrite rules applied in this step -- 
apply :: Tree RValue -> Rules -> IO ([Tree RValue], Integer)
apply rval rules = case searchPatterns rval rules of 
        Just (binding, templ) -> templateOrRecurse True binding templ
        Nothing -> templateOrRecurse False mempty undefined
    where
        templateOrRecurse :: Bool -> M.Map T.Text [Tree RValue] -> [Tree RValue] -> IO ([Tree RValue], Integer)
        -- We got a successful branch pattern match, let's rewrite to the template.
        templateOrRecurse True bindings templates = do
            treeLists <- mapM (betaReduce bindings) templates
            pure (concat treeLists, 1)
        templateOrRecurse False _ _ = case rval of
            -- If we failed to match a nub branch, we give it back unchanged
            Branch [] -> pure ([rval], 0)
            -- If we failed to match an inhabited branch, let's match the branch's children
            Branch rtrees -> do
                treeList <- mapM (`apply` rules) rtrees
                let (rtrees', count) = foldr (\(rtree, apCount) (accTree, accCount) -> (rtree:accTree, apCount+accCount)) ([], 0) treeList
                pure ([Branch . concat $ rtrees'], count)
            -- If we failed to match a single runtime value, we give it back unchanged
            Leaf _ -> pure ([rval], 0)

-- Apply variable bindings to a pattern, "filling it out" and discarding the Pattern type information
-- TODO: this function can bottom, let's return errors with a proper monad!
--            Variable name <-> value        
betaReduce :: M.Map T.Text [Tree RValue] -> Tree RValue -> IO [Tree RValue]
betaReduce bindings (Branch trees) = do
    treeLists <- mapM (betaReduce bindings) trees
    pure [Branch $ concat treeLists]
betaReduce bindings (Leaf (PVariable pvar))
    -- Handle special accumulators
    | T.head pvar == '?' =
        case pvar of
            -- sum accumulator 
            "?+" -> pure $ case bindings M.!? pvar of
                Just rvals -> [Leaf . RNumber . sum $ ((\case { Leaf (RNumber rnum) -> rnum ; _ -> 0 }) <$> rvals)]
                Nothing -> []
            -- product accumulator 
            "?*" -> pure $ case bindings M.!? pvar of
                Just rvals -> [Leaf . RNumber . product $ ((\case { Leaf (RNumber rnum) -> rnum ; _ -> 0 }) <$> rvals)]
                Nothing -> []
            -- negation accumulator 
            "?-" -> pure $ case bindings M.!? pvar of
                Just rvals -> (\case { Leaf (RNumber rnum) -> Leaf . RNumber $ -rnum ; x -> x }) <$> rvals
                Nothing -> []
            -- output accumulator
            "?>" -> case bindings M.!? pvar of
                Just rvals -> (putStrLn . unwords $ sexprprint <$> rvals) >> pure rvals
                Nothing -> pure []
            -- handle all other accumulators as normal vars -- their behavior happens at matchtime 
            _ -> pure $ case bindings M.!? pvar of
                Just rvals -> reverse rvals
                Nothing -> error . T.unpack $ T.append "Missing binding for special accumulator " pvar
    -- Substitute pattern variable normally
    | otherwise = pure $ case bindings M.!? pvar of
        Just rvals -> reverse rvals
        Nothing -> error . T.unpack $ T.append "Missing binding for variable " pvar
betaReduce _ (Leaf pval) = pure [Leaf pval]


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
    -- TODO: make this more efficient
    Rules rewrites <> Rules rewrites' = Rules (rewrites ++ rewrites')

instance Monoid Rules where
    mempty = Rules []


-- Left if no rewrites applied
-- Right the new runtime values if a rewrite applied
applyRewrites :: Tree RValue -> Rules -> IO (Either [Tree RValue] [Tree RValue])
applyRewrites rval rules = maybe (Left [rval]) Right <$> _applyRewrites rval rules

_applyRewrites :: Tree RValue -> Rules -> IO (Maybe [Tree RValue])
_applyRewrites rval rules = apply rval rules <&> (\case
            (_, 0) -> Nothing
            (rvals', _) -> Just rvals')
