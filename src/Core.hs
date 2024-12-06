{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
module Core where

import qualified Data.Text as T
import Data.List ( intercalate )
import qualified Data.Map as M
import Control.Monad.Trans.State.Lazy ( State, modify, runState, gets, StateT )
import Control.Monad (zipWithM)
import qualified Language.Haskell.TH.Syntax as TH
import Data.Functor ((<&>), void)
import qualified Data.Text.ICU as ICU
import Data.Maybe (catMaybes, mapMaybe, fromJust)
import Data.Bifunctor (first, Bifunctor (second))
import Control.Monad.Trans.State (get, StateT (..))
import Control.Monad.Trans.State (put)
import Control.Monad.Trans.Class (lift)

instance Eq ICU.Regex where
    a == b = show a == show b

instance TH.Lift ICU.Regex where
    liftTyped = undefined

------------------------------------------------------------
-- Runtime values, including pattern variables and the tree
------------------------------------------------------------

-- Enum for special accumulators
data SpecialAccumTag = SASum | SANegate | SAProduct | SAOutput | SAInput | SAPack | SAUnpack deriving (TH.Lift, Eq)
instance Show SpecialAccumTag where
    show SASum     = "+"
    show SANegate  = "-"
    show SAProduct = "*"
    show SAOutput  = ">"
    show SAInput   = "<"
    show SAPack    = "@"
    show SAUnpack  = "%"

-- Pattern variable tags, holding the origin-type of the pattern variable and any special data it needs to operate
data PVarTag = PVarNothingSpecial | PVarSpecialAccum SpecialAccumTag | PVarRegexGroup deriving (TH.Lift, Eq)

-- Pattern values. The PVar record holds values that need to be tracked for all pattern variables. 
-- Currently this data includes 
--    * its matching-strategy ("tag"),
--    * its eagerness (can it match against a tree with other matching rules?),
--    * and its name
data PVar = PVar {
    pvarEager :: Bool,
    pvarTag :: PVarTag,
    pvarName :: T.Text
} deriving (TH.Lift, Eq)

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
data RValue = RSymbol T.Text | RString T.Text | RRegex ICU.Regex | RNumber Integer | RVariable PVar deriving (TH.Lift, Eq)

instance Show RValue where
    show (RSymbol t) = T.unpack t
    show (RString t) = T.unpack . T.concat $ ["\"", t, "\""]
    show (RRegex t) = T.unpack . T.concat $ ["/", ICU.pattern t, "/"]
    show (RNumber t) = show t
    show (RVariable t) = show t

-- The tree!
data Tree a = Branch [Tree a] | Leaf a deriving (TH.Lift, Functor, Foldable, Traversable, Eq)

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
data Rewrite a = Rewrite {
    -- The pattern to match in the data tree
    rewritePattern :: Tree a,
    -- The template to replace the matched pattern with
    rewriteTemplate :: [Tree a]
} deriving (TH.Lift)

instance Show a => Show (Rewrite a) where
    show (Rewrite pattern templates) = sexprprint pattern ++ " -to-> " ++ unwords (sexprprint <$> templates)



-- The Binder type, holding the intermediate state needed to apply a single rewrite rule
data Binder = Binder {
    binderTreeBindings :: M.Map T.Text [Tree RValue],
    binderRegexBindings :: M.Map T.Text T.Text
}

emptyBinder :: Binder
emptyBinder = Binder {
    binderTreeBindings = mempty,
    binderRegexBindings = mempty
}

-- Binder lenses --
updateBinderTreeBindings :: (M.Map T.Text [Tree RValue] -> M.Map T.Text [Tree RValue]) -> Binder -> Binder
updateBinderTreeBindings f b = b { binderTreeBindings = f (binderTreeBindings b) }
updateBinderRegexBindings :: (M.Map T.Text T.Text -> M.Map T.Text T.Text) -> Binder -> Binder
updateBinderRegexBindings f b = b { binderRegexBindings = f (binderRegexBindings b) }

-- What name do we use for PVars that go into the Binder? 
pvarBinderName :: PVar -> T.Text
pvarBinderName pvar = if pvarRegexGroupAcceptor pvar 
    then pvarName pvar
    else T.cons (T.head $ pvarSigil pvar) (pvarName pvar)

-- Bind a new or existing tree pattern variable. If the variable is already bound, tack onto its binding list.
addTreeBinding :: Monad m => PVar -> Tree RValue -> StateT Binder m ()
addTreeBinding pvar binding = modify (updateBinderTreeBindings $ M.alter go (pvarBinderName pvar))
    where
        go Nothing = Just [binding]
        go (Just existingBindings) = Just $ binding:existingBindings

-- Get the binding list for a tree pattern variable
getTreeBinding :: Monad m => PVar -> StateT Binder m (Maybe [Tree RValue])
getTreeBinding pvar = gets (M.lookup (pvarBinderName pvar) . binderTreeBindings)

-- Bind a new or existing tree pattern variable. Succeed if the binding completed, which requires equality with
-- the existing binding.
bindIfEqual :: Monad m => PVar -> Tree RValue -> StateT Binder m Bool
bindIfEqual pvar binding = do
    prevBinding <- gets (M.lookup (pvarBinderName pvar) . binderTreeBindings)
    case prevBinding of
        Just (rval:_) -> pure $ binding == rval
        _ -> addTreeBinding pvar binding >> pure True

-- Bind a new or existing regex match variable. Overwrite previously bound variables.
addRegexBinding :: (Monad m) => T.Text -> T.Text -> StateT Binder m ()
addRegexBinding groupname matchtext = modify (updateBinderRegexBindings $ M.insert groupname matchtext)

-- Get the binding list for a tree pattern variable
getRegexBinding :: Monad m => PVar -> StateT Binder m (Maybe T.Text)
getRegexBinding pvar = gets (M.lookup (pvarBinderName pvar) . binderRegexBindings)

-- Get the correct binding for a PVar. Some PVars may be stored differently in the Binder depending on their tag.
getBinding :: Monad m => PVar -> StateT Binder m (Maybe [Tree RValue])
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
newtype Rules = Rules {
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
tryApply :: Rules                                   -- All the rewrite rules known in the environment, for eager matching
         -> Tree RValue                             -- Input tree
         -> Tree RValue                             -- Pattern to match
         -> State Binder Bool -- Updated variable bindings, along with whether the match succeeded
-- Bind pattern variables and special accumulators
tryApply rules rval (Leaf (RVariable pvar)) = do
    -- Check for submatches and fail out if we're matching an eager variable 
    if pvarEager pvar && bfsPatterns rval rules
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
               -- output accumulator
               SAOutput -> addTreeBinding pvar rval >> pure True
               -- input accumulator
               SAInput -> error "Unimplemented input accumulator :?<"
               -- cons to sexpr (pack) eager accumulator 
               SAPack -> if bfsPatterns rval rules
                       then pure False
                       else (addTreeBinding pvar . deepFlatten $ case rval of
                           Leaf r -> [Leaf r]
                           Branch rs -> rs) >> pure True
               -- sexpr to cons (unpack) eager accumulator 
               SAUnpack -> case rval of
                   Leaf _ -> pure False
                   Branch rs -> if bfsPatterns rval rules
                       then pure False
                       else (addTreeBinding pvar . foldr (\leaf acc -> Branch [leaf, acc]) (Branch []) $ rs) >> pure True
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

fst3 :: (a, b, c) -> a
fst3 (a,_,_)=a

-- Check the tip of the input tree, attempting to apply all rewrite rules at the tip
-- On a successful rule match, the state holds the pattern variable bindings and we give back the template
searchPatterns :: Tree RValue -> Rules -> StateT Binder Maybe [Tree RValue]
searchPatterns rval rr@(Rules rules) = let
    patterns = rewritePattern <$> rules
    templates = rewriteTemplate <$> rules
    makeMatchAttempts = tryApply rr rval <$> patterns
    matchAttemptsWithTemplates = zipWith (\(success, binding) template -> (success,binding,template)) (flip runState emptyBinder <$> makeMatchAttempts) templates
    (_, possibleSuccess) = break fst3 matchAttemptsWithTemplates
    in case possibleSuccess of
        (_, binding, templ):_ -> put binding >> pure templ
        [] -> lift Nothing

-- BFS the input tree to try applying all rewrite rules anywhere it's possible.
-- Similar to `apply`, just with no rewriting happening. Used in eager matching
bfsPatterns :: Tree RValue -> Rules -> Bool
bfsPatterns rval rules = case runStateT (searchPatterns rval rules) emptyBinder of
    Just _ -> True
    Nothing -> case rval of
        -- We failed to match a nub branch
        Branch [] -> False
        -- We failed to match a single runtime value
        Leaf _ -> False
        -- We failed to match an inhabited branch, let's BFS
        Branch rtrees -> any (`bfsPatterns` rules) rtrees

-- Apply variable bindings to a pattern, "filling it out" and discarding the Pattern type information
-- TODO: this function can bottom, let's return errors with a proper monad!
betaReduce :: Tree RValue -> StateT Binder IO [Tree RValue]
betaReduce (Branch trees) = do
    treeLists <- mapM betaReduce trees
    pure [Branch $ concat treeLists]
betaReduce (Leaf (RVariable pvar)) = do
    pvarBinding <- getBinding pvar
    -- Handle special accumulators
    if pvarSpecialAccumAcceptor pvar
        then lift $ goSpecialAccums pvarBinding
        -- Handle substitution on normal pattern variables
        else case pvarBinding of
            Just rvals -> pure $ reverse rvals
            Nothing -> fail $ "Missing binding for variable " ++ show pvar
    where
        goSpecialAccums :: Maybe [Tree RValue] -> IO [Tree RValue]
        goSpecialAccums Nothing = pure [] -- unbound special accumulators produce nothing
        goSpecialAccums (Just rvals) = case fromJust $ pvarSpecialAccumTag pvar of
            -- sum accumulator 
            SASum -> pure [Leaf . RNumber . sum $ ((\case { Leaf (RNumber rnum) -> rnum ; _ -> 0 }) <$> rvals)]
            -- product accumulator 
            SAProduct -> pure [Leaf . RNumber . product $ ((\case { Leaf (RNumber rnum) -> rnum ; _ -> 1 }) <$> rvals)]
            -- negation accumulator 
            SANegate -> pure $ (\case { Leaf (RNumber rnum) -> Leaf . RNumber $ -rnum ; x -> x }) <$> reverse rvals
            -- output accumulator
            SAOutput -> (putStrLn . unwords . reverse $ sexprprint <$> rvals) >> pure rvals
            SAInput -> undefined -- TODO
            -- other accumulators (SAPack, SAUnpack) bind like normal variables
            _ -> pure $ reverse rvals
-- Perform regex capture group substitutions!
betaReduce (Leaf (RString pstr)) = do
    regexBindings <- gets (fmap (first (T.cons '$')) . M.toList . binderRegexBindings)
    pure . pure . Leaf . RString $ foldr (uncurry T.replace) pstr regexBindings
betaReduce (Leaf pval) = pure [Leaf pval]


-- BFS the input tree to try applying all rewrite rules anywhere it's possible. Similar to `bfsPatterns`. --
-- Evaluates to the new tree and the number of rewrite rules applied in this step -- 
apply :: Tree RValue -> Rules -> IO ([Tree RValue], Int)
apply rval rules = case runStateT (searchPatterns rval rules) emptyBinder of
    Just (template, binder) -> do
        -- We found a match! Let's inject the variables
        (treeLists, _) <- runStateT (mapM betaReduce template) binder
        pure (concat treeLists, 1)
    Nothing -> case rval of
        -- If we failed to match a nub branch, we give it back unchanged
        Branch [] -> pure ([rval], 0)
        -- If we failed to match an inhabited branch, let's match the branch's children
        Branch rtrees -> do
            treeList <- mapM (`apply` rules) rtrees
            let (rtrees', count) = foldr (\(rtree, apCount) (accTree, accCount) -> (rtree:accTree, apCount+accCount)) ([], 0) treeList
            pure ([Branch . concat $ rtrees'], count)
        -- If we failed to match a single runtime value, we give it back unchanged
        Leaf _ -> pure ([rval], 0)


-- instance Semigroup Rules where
--     -- TODO: make this more efficient
--     Rules rewrites <> Rules rewrites' = Rules (rewrites ++ rewrites')

-- instance Monoid Rules where
--     mempty = Rules []


-- Left if no rewrites applied
-- Right the new runtime values if a rewrite applied
-- applyRewrites :: Tree RValue -> Rules -> IO (Either [Tree RValue] [Tree RValue])
-- applyRewrites rval rules = maybe (Left [rval]) Right <$> _applyRewrites rval rules

-- _applyRewrites :: Tree RValue -> Rules -> IO (Maybe [Tree RValue])
-- _applyRewrites rval rules = apply rval rules <&> (\case
--             (_, 0) -> Nothing
--             (rvals', _) -> Just rvals')
