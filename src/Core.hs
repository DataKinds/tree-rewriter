{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
module Core where

import qualified Data.Text as T
import Data.List ( intercalate )
import qualified Data.Map as M
import Control.Monad.Trans.State.Lazy ( State, modify, runState, gets )
import Control.Monad (zipWithM)
import Language.Haskell.TH.Syntax
import Data.Functor ((<&>))
import qualified Data.Text.ICU as ICU
import Data.Maybe (catMaybes, mapMaybe, fromJust)
import Data.Bifunctor (first, Bifunctor (second))

instance Eq ICU.Regex where
    a == b = show a == show b

instance Lift ICU.Regex where
    liftTyped = undefined

------------------------------------------------------------
-- Runtime values, including pattern variables and the tree
------------------------------------------------------------

-- Enum for special accumulators
data SpecialAccumTag = SASum | SANegate | SAProduct | SAOutput | SAInput | SAPack | SAUnpack deriving (Lift, Eq)
instance Show SpecialAccumTag where
    show SASum     = "+"
    show SANegate  = "-"
    show SAProduct = "*"
    show SAOutput  = ">"
    show SAInput   = "<"
    show SAPack    = "@"
    show SAUnpack  = "%"

-- Pattern variable tags, holding the origin-type of the pattern variable and any special data it needs to operate
data PVarTag = PVarNothingSpecial | PVarSpecialAccum SpecialAccumTag | PVarRegexGroup deriving (Lift, Eq)

-- Pattern values. The PVar record holds values that need to be tracked for all pattern variables. 
-- Currently this data includes 
--    * its matching-strategy ("tag"),
--    * its eagerness (can it match against a tree with other matching rules?),
--    * and its name
data PVar = PVar {
    pvarEager :: Bool,
    pvarTag :: PVarTag,
    pvarName :: T.Text
} deriving (Lift, Eq)

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
data RValue = RSymbol T.Text | RString T.Text | RRegex ICU.Regex | RNumber Integer | RVariable PVar deriving (Lift, Eq)

instance Show RValue where
    show (RSymbol t) = T.unpack t
    show (RString t) = T.unpack . T.concat $ ["\"", t, "\""]
    show (RRegex t) = T.unpack . T.concat $ ["/", ICU.pattern t, "/"]
    show (RNumber t) = show t
    show (RVariable t) = show t

-- The tree!
data Tree a = Branch [Tree a] | Leaf a deriving (Lift, Functor, Foldable, Traversable, Eq)

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

data Binder = Binder {
    binderTreeBindings :: M.Map T.Text [Tree RValue]
}

-- takes a name and a runtime value tree, creates a new binding or appends to a binding list
addBinding :: (Ord k) => k -> a -> State (M.Map k [a]) Bool
addBinding name binding = modify (M.alter (pure . \case { Nothing -> [binding] ; (Just bindings) -> binding:bindings }) name) >> pure True
-- takes a name and a runtime value, succeeds if the new binding equals the old one
bindIfEqual :: (Ord k, Eq a) => k -> a -> State (M.Map k [a]) Bool
bindIfEqual name binding = do
    prevBinding <- gets (M.lookup name)
    case prevBinding of
        Just [] -> error "Unexpected empty binding"
        Just (rval:_) -> pure $ binding == rval
        Nothing -> modify (M.insert name [binding]) >> pure True

-- ‧͙⁺˚*･༓☾ Try to match a single pattern at the tip of a tree ☽༓･*˚⁺‧͙ --
-- Statefully return the variables bound on a successful application --
tryApply :: Rules                                   -- All the rewrite rules known in the environment, for eager matching
         -> Tree RValue                             -- Input tree
         -> Tree RValue                             -- Pattern to match
         -> State (M.Map T.Text [Tree RValue]) Bool -- Updated variable bindings, along with whether the match succeeded
-- Bind pattern variables and special accumulators
tryApply rules rval (Leaf (RVariable pvar))
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
        -- cons to sexpr (pack) eager accumulator 
        "?!@" -> let
                deepCollect :: [Tree RValue] -> [Tree RValue]
                deepCollect [] = []
                deepCollect (val:(Leaf end):_) = [val, Leaf end]
                deepCollect (val:(Branch rest):_) = val : deepCollect rest
                deepCollect [end] = [end]
            in if bfsPatterns rval rules then pure False else addBinding pvar . Branch . deepCollect $ case rval of
                    Leaf r -> [Leaf r]
                    Branch rs -> rs
        -- sexpr to cons (unpack) eager accumulator 
        "?!%" -> case rval of
            Leaf _ -> pure False
            Branch rs -> if bfsPatterns rval rules then pure False else addBinding pvar . foldr (\leaf acc -> Branch [leaf, acc]) (Branch []) $ rs
        _ -> error . T.unpack $ T.append "Tried binding unknown special accumulator " pvar
    -- Bind eager variable (aka: a var that only binds with a term that cannot be rewritten)
    | T.head pvar == '!' = if bfsPatterns rval rules then pure False else bindIfEqual (T.tail pvar) rval
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
                namedCaptures = first (T.cons '$' . T.pack . show) <$> captures
            mapM_ (\(name, capture) -> addBinding name (Leaf $ RString capture)) namedCaptures
            addBinding "$<" (Leaf $ RString preMatch)
            addBinding "$>" (Leaf $ RString postMatch)
-- Catch failed matches
tryApply _ _ _ = pure False

fst3 :: (a, b, c) -> a
fst3 (a,_,_)=a

-- Check the tip of the input tree, attempting to apply all rewrite rules at the tip
-- On a successful rule match, give back the pattern variable bindings and the template
searchPatterns :: Tree RValue -> Rules -> Maybe (M.Map T.Text [Tree RValue], [Tree RValue])
searchPatterns rval rr@(Rules rules) = let
    patterns = rewritePattern <$> rules
    templates = rewriteTemplate <$> rules
    makeMatchAttempts = tryApply rr rval <$> patterns
    matchAttemptsWithTemplates = zipWith (\(success, binding) template -> (success,binding,template)) (flip runState mempty <$> makeMatchAttempts) templates
    (_, possibleSuccess) = break fst3 matchAttemptsWithTemplates
    in case possibleSuccess of
        (_, binding, templ):_ -> Just (binding, templ)
        [] -> Nothing

-- BFS the input tree to try applying all rewrite rules anywhere it's possible.
-- Similar to `apply`, just with no rewriting happening. Used in eager matching
bfsPatterns :: Tree RValue -> Rules -> Bool
bfsPatterns rval rules = case searchPatterns rval rules of
    Just _ -> True
    Nothing -> case rval of
        -- We failed to match a nub branch
        Branch [] -> False
        -- We failed to match a single runtime value
        Leaf _ -> False
        -- We failed to match an inhabited branch, let's BFS
        Branch rtrees -> any (`bfsPatterns` rules) rtrees

-- BFS the input tree to try applying all rewrite rules anywhere it's possible. Similar to `bfsPatterns`. --
-- Evaluates to the new tree and the number of rewrite rules applied in this step -- 
apply :: Tree RValue -> Rules -> IO ([Tree RValue], Integer)
apply rval rules = case searchPatterns rval rules of
    Just (binding, templ) -> do
        -- We found a match! Let's inject the variables
        treeLists <- mapM (betaReduce binding) templ
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

-- Apply variable bindings to a pattern, "filling it out" and discarding the Pattern type information
-- TODO: this function can bottom, let's return errors with a proper monad!
--            Variable name <-> value        
-- TODO: we should pass this function regex group bindings too!
betaReduce :: M.Map T.Text [Tree RValue] -> Tree RValue -> IO [Tree RValue]
betaReduce bindings (Branch trees) = do
    treeLists <- mapM (betaReduce bindings) trees
    pure [Branch $ concat treeLists]
betaReduce bindings (Leaf (RVariable pvar))
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
                Just rvals -> (putStrLn . unwords . reverse $ sexprprint <$> rvals) >> pure rvals
                Nothing -> pure []
            -- handle all other accumulators as normal vars -- their behavior happens at matchtime 
            _ -> pure $ case bindings M.!? pvar of
                Just rvals -> reverse rvals
                Nothing -> error . T.unpack $ T.append "Missing binding for special accumulator " pvar
    -- Substitute pattern variable normally
    | otherwise = pure $ case bindings M.!? pvar of
        Just rvals -> reverse rvals
        Nothing -> error . T.unpack $ T.append "Missing binding for variable " pvar
-- Perform regex capture group substitutions!
betaReduce bindings (Leaf (RString pstr)) = let
    isRegexCaptureBinding name (Leaf (RString _):_) = T.head name == '$'
    isRegexCaptureBinding _ _ = False
    captureBindings = second (\((Leaf (RString capGroup)):_) -> capGroup) <$> M.toList (M.filterWithKey isRegexCaptureBinding bindings)
    in pure . pure . Leaf . RString $ foldr (uncurry T.replace) pstr captureBindings
betaReduce _ (Leaf pval) = pure [Leaf pval]


-- Rule list used for execution
newtype Rules = Rules [Rewrite RValue]

unrules :: Rules -> [Rewrite RValue]
unrules (Rules rewrites) = rewrites

instance Show Rules where
    show (Rules rewrites) = unlines . concat $ [
        ["+---------------+"],
        ["| Rewrite rules |"],
        ["+---------------+"],
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
