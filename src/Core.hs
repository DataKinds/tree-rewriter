{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Core where

import qualified Data.Text as T
import Data.List ( intercalate )
import qualified Data.Map as M
import Control.Monad.Trans.State.Lazy ( State, gets, StateT )
import Control.Monad (zipWithM)
import qualified Language.Haskell.TH.Syntax as TH
import qualified Data.Text.ICU as ICU
import Data.Maybe (mapMaybe, fromJust)
import Data.Bifunctor (first)
import Data.Function (on)
import Data.Functor.Identity (Identity(..))
import Optics (modifying, makeFieldLabelsNoPrefix)

instance Eq ICU.Regex where
    (==) = (==) `on` show

instance Ord ICU.Regex where
    compare = compare `on` show

instance TH.Lift ICU.Regex where
    liftTyped = undefined

------------------------------------------------------------
-- Runtime values, including pattern variables and the tree
------------------------------------------------------------

-- Enum for special accumulators
data SpecialAccumTag = SASum | SANegate | SAProduct | SAPack | SAUnpack deriving (TH.Lift, Eq, Ord)
instance Show SpecialAccumTag where
    show SASum     = "+"
    show SANegate  = "-"
    show SAProduct = "*"
    show SAPack    = "@"
    show SAUnpack  = "%"

-- Pattern variable tags, holding the origin-type of the pattern variable and any special data it needs to operate
data PVarTag = PVarNothingSpecial | PVarSpecialAccum SpecialAccumTag | PVarRegexGroup deriving (TH.Lift, Eq, Ord)

-- Pattern values. The PVar record holds values that need to be tracked for all pattern variables. 
-- Currently this data includes 
--    * its matching-strategy ("tag"),
--    * its eagerness (can it match against a tree with other matching rules?),
--    * and its name
data PVar = PVar {
    pvarEager :: Bool,
    pvarTag :: PVarTag,
    pvarName :: T.Text
} deriving (TH.Lift, Eq, Ord)

-- Decision functions for many characteristics of PVars
pvarSpecialAccumAcceptor :: PVar -> Bool
pvarSpecialAccumAcceptor = (\case { PVarSpecialAccum _ -> True ; _ -> False }) . pvarTag
pvarRegexGroupAcceptor :: PVar -> Bool
pvarRegexGroupAcceptor = (\case { PVarRegexGroup -> True ; _ -> False }) . pvarTag

pvarSpecialAccumTag :: PVar -> Maybe SpecialAccumTag
pvarSpecialAccumTag = (\case { PVarSpecialAccum saTag -> Just saTag ; _ -> Nothing }) . pvarTag

-- Given a PVar, what sequence of sigils is it spelled with?
pvarSigil :: PVar -> T.Text
pvarSigil (PVar eager tag _) = T.pack $ go tag:eagerSigil
    where
        eagerSigil = if eager then "!" else ""
        go PVarNothingSpecial = ':'
        go (PVarSpecialAccum _) = '?'
        go PVarRegexGroup = '$'

instance Show PVar where
    show pvar = T.unpack . T.concat $ [pvarSigil pvar, pvarName pvar]

-- Runtime values. This is the structure that Rosin trees are parameterized over: the "leaf type".
data RValue = RSymbol T.Text | RString T.Text | RRegex ICU.Regex | RNumber Integer | RVariable PVar deriving (TH.Lift, Eq, Ord)
instance Show RValue where
    show (RSymbol t) = T.unpack t
    show (RString t) = T.unpack . T.concat $ ["\"", t, "\""]
    show (RRegex t) = T.unpack . T.concat $ ["/", ICU.pattern t, "/"]
    show (RNumber t) = show t
    show (RVariable t) = show t



-- The tree!
data Tree a = Branch [Tree a] | Leaf a deriving (TH.Lift, Functor, Foldable, Traversable, Eq, Ord)

-- Unwrap one level of branching, if it's possible
unbranch :: Tree RValue -> [Tree RValue]
unbranch (Branch trees) = trees
unbranch leaf = [leaf]

-- Rewraps a list of trees into one tree, dropping unnecesary nesting
rebranch :: [Tree RValue] -> Tree RValue
rebranch [t] = t
rebranch ts = Branch ts

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
    show = sexprprint

-- Tree rewrite rule datatype
-- Parameterized on leaf type
data Rewrite a = Rewrite {
    -- The pattern to match in the data tree
    rewritePattern :: Tree a,
    -- The template to replace the matched pattern with
    rewriteTemplate :: [Tree a]
} deriving (TH.Lift, Eq)

instance Show a => Show (Rewrite a) where
    show (Rewrite pattern templates) = sexprprint pattern ++ " -to-> " ++ unwords (sexprprint <$> templates)



-- The Binder type, holding the intermediate state needed to apply a single rewrite rule
data Binder = Binder {
    treeBindings :: M.Map T.Text [Tree RValue],
    regexBindings :: M.Map T.Text T.Text
}
makeFieldLabelsNoPrefix ''Binder
type BinderT = StateT Binder -- Binder monad
type Binder_ = BinderT Identity

emptyBinder :: Binder
emptyBinder = Binder {
    treeBindings = mempty,
    regexBindings = mempty
}

-- What name do we use for PVars that go into the Binder? 
pvarBinderName :: PVar -> T.Text
pvarBinderName pvar = if pvarRegexGroupAcceptor pvar
    then pvarName pvar
    else T.cons (T.head $ pvarSigil pvar) (pvarName pvar)

-- Bind a new or existing tree pattern variable. If the variable is already bound, tack onto its binding list.
addTreeBinding :: Monad m => PVar -> Tree RValue -> BinderT m ()
addTreeBinding pvar binding = modifying #treeBindings (M.alter go (pvarBinderName pvar))
    where
        go Nothing = Just [binding]
        go (Just existingBindings) = Just $ binding:existingBindings

-- Get the binding list for a tree pattern variable
getTreeBinding :: Monad m => PVar -> BinderT m (Maybe [Tree RValue])
getTreeBinding pvar = gets (M.lookup (pvarBinderName pvar) . treeBindings)

-- Bind a new or existing tree pattern variable. Succeed if the binding completed, which requires equality with
-- the existing binding.
bindIfEqual :: Monad m => PVar -> Tree RValue -> BinderT m Bool
bindIfEqual pvar binding = do
    prevBinding <- gets (M.lookup (pvarBinderName pvar) . treeBindings)
    case prevBinding of
        Just (rval:_) -> pure $ binding == rval
        _ -> addTreeBinding pvar binding >> pure True

-- Bind a new or existing regex match variable. Overwrite previously bound variables.
addRegexBinding :: (Monad m) => T.Text -> T.Text -> BinderT m ()
addRegexBinding groupname matchtext = modifying #regexBindings $ M.insert groupname matchtext

-- Get the binding list for a tree pattern variable
getRegexBinding :: Monad m => PVar -> BinderT m (Maybe T.Text)
getRegexBinding pvar = gets (M.lookup (pvarBinderName pvar) . regexBindings)

-- Get the correct binding for a PVar. Some PVars may be stored differently in the Binder depending on their tag.
getBinding :: Monad m => PVar -> BinderT m (Maybe [Tree RValue])
getBinding pvar = case pvarTag pvar of
    PVarRegexGroup -> do
        binding <- getRegexBinding pvar
        pure (pure . Leaf . RString <$> binding)
    _ -> getTreeBinding pvar


-- Flatten a bunch of RValue trees into one Tree Branch in DFS order
deepFlatten :: [Tree a] -> Tree a
deepFlatten = Branch . go
    where go [] = []
          go [end] = [end]
          go (val:(Leaf end):_) = [val, Leaf end]
          go (val:(Branch rest):_) = val : go rest

-- Construct a PVar for a regex group with a given name
makeRegexPVar :: T.Text -> PVar
makeRegexPVar name = PVar {
    pvarEager = False,
    pvarTag = PVarRegexGroup,
    pvarName = name
}

-- List of rules for mutating the data tree
newtype Rules = Rules { -- TODO: Remove
    unrules :: [Rewrite RValue]
}

instance Show Rules where
    show (Rules rewrites) = unlines . concat $ [
        ["+---------------+"],
        ["| Rewrite rules |"],
        ["+---------------+"],
        show <$> rewrites]


-- ‧͙⁺˚*･༓☾ Try to match a single pattern at the tip of a tree ☽༓･*˚⁺‧͙ --
-- Statefully return the variables bound on a successful application --
tryApply :: (Tree RValue -> Bool)                   -- Eager matcher: does a given tree match anything else?
         -> Tree RValue                             -- Input tree
         -> Tree RValue                             -- Pattern to match
         -> Binder_ Bool                            -- Updated variable bindings, along with whether the match succeeded
-- Bind pattern variables and special accumulators
tryApply submatcher rval (Leaf (RVariable pvar)) = do
    -- Check for submatches and fail out if we're matching an eager variable 
    if pvarEager pvar && submatcher rval
        then pure False
        else go
    where
        go :: State Binder Bool
        go | pvarSpecialAccumAcceptor pvar = case fromJust $ pvarSpecialAccumTag pvar of
           -- Bind special accumulators
               -- sum accumulator 
               SASum -> case rval of
                   num@(Leaf (RNumber _)) -> addTreeBinding pvar num >> pure True
                   _ -> pure False
               -- product accumulator 
               SAProduct -> case rval of
                   num@(Leaf (RNumber _)) -> addTreeBinding pvar num >> pure True
                   _ -> pure False
               -- negation accumulator 
               SANegate -> case rval of
                   num@(Leaf (RNumber _)) -> addTreeBinding pvar num >> pure True
                   _ -> pure False
               -- cons to sexpr (pack) accumulator 
               SAPack -> (addTreeBinding pvar . deepFlatten $ case rval of
                           Leaf r -> [Leaf r]
                           Branch rs -> rs) >> pure True
               -- sexpr to cons (unpack) accumulator 
               SAUnpack -> case rval of
                   Leaf _ -> pure False
                   Branch rs -> (addTreeBinding pvar . foldr (\leaf acc -> Branch [leaf, acc]) (Branch []) $ rs) >> pure True
           -- Bind regular pattern variable
           | otherwise = bindIfEqual pvar rval
-- Match ntree branch patterns exactly
tryApply _ (Branch []) (Branch []) = pure True
tryApply _ (Branch []) _ = pure False
tryApply rules (Branch rtrees) (Branch pvals)
    -- No point to checking patterns that match branches of the wrong length
    | length rtrees /= length pvals = pure False
    | otherwise = and <$> zipWithM (tryApply rules) rtrees pvals
-- Match symbol patterns
tryApply _ (Leaf (RSymbol rsym)) (Leaf (RSymbol psym))
    | rsym == psym = pure True
    | otherwise = pure False
-- Match number patterns
tryApply _ (Leaf (RNumber rnum)) (Leaf (RNumber pnum))
    | rnum == pnum = pure True
    | otherwise = pure False
-- Match string patterns 
tryApply _ (Leaf (RString rstr)) (Leaf (RString pstr))
    | rstr == pstr = pure True
    | otherwise = pure False
-- Match regex, setting variables corresponding to all captures
tryApply _ (Leaf (RString rstr)) (Leaf (RRegex preg)) =
    case ICU.find preg rstr of
        Nothing -> pure False
        Just match -> do
            let count = ICU.groupCount match
                preMatch = fromJust $ ICU.prefix 0 match
                postMatch = fromJust $ ICU.suffix 0 match
                captures = mapMaybe (\idx -> ICU.group idx match >>= (\m -> pure (idx, m))) [0..count]
                namedCaptures = first (T.pack . show) <$> captures
            mapM_ (uncurry addRegexBinding) namedCaptures
            addRegexBinding "<" preMatch
            addRegexBinding ">" postMatch
            pure True
-- Catch failed matches
tryApply _ _ _ = pure False


-- Apply variable bindings to a pattern, "filling it out" and discarding the Pattern type information
betaReduce :: Tree RValue -> Binder_ [Tree RValue]
betaReduce (Branch trees) = do
    treeLists <- mapM betaReduce trees
    pure [Branch $ concat treeLists]
betaReduce input@(Leaf (RVariable pvar)) = do
    pvarBinding <- getBinding pvar
    -- Handle special accumulators
    if pvarSpecialAccumAcceptor pvar
        then pure $ goSpecialAccums pvarBinding
        -- Handle substitution on normal pattern variables
        else case pvarBinding of
            Just rvals -> pure $ reverse rvals
            Nothing -> pure [input]
    where
        goSpecialAccums :: Maybe [Tree RValue] -> [Tree RValue]
        goSpecialAccums Nothing = [] -- unbound special accumulators produce nothing
        goSpecialAccums (Just rvals) = case fromJust $ pvarSpecialAccumTag pvar of
            -- sum accumulator 
            SASum -> [Leaf . RNumber . sum $ ((\case { Leaf (RNumber rnum) -> rnum ; _ -> 0 }) <$> rvals)]
            -- product accumulator 
            SAProduct -> [Leaf . RNumber . product $ ((\case { Leaf (RNumber rnum) -> rnum ; _ -> 1 }) <$> rvals)]
            -- negation accumulator 
            SANegate -> (\case { Leaf (RNumber rnum) -> Leaf . RNumber $ -rnum ; x -> x }) <$> reverse rvals
            -- other accumulators (SAPack, SAUnpack) bind like normal variables
            _ -> reverse rvals
-- Perform regex capture group substitutions!
betaReduce (Leaf (RString pstr)) = do
    regexBindings <- gets (fmap (first (T.cons '$')) . M.toList . regexBindings)
    pure . pure . Leaf . RString $ foldr (uncurry T.replace) pstr regexBindings
betaReduce (Leaf pval) = pure [Leaf pval]