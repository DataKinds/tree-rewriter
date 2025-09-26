{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}

module Core where

import qualified Data.Text as T
import Data.List ( intercalate )
import qualified Data.Map as M
import Control.Monad.Trans.State.Lazy ( State, StateT, runStateT )
import Control.Monad (zipWithM)
import qualified Language.Haskell.TH.Syntax as TH
import qualified Data.Text.ICU as ICU
import Data.Maybe (mapMaybe, fromJust, fromMaybe)
import Data.Bifunctor (first, Bifunctor (..))
import Data.Function (on)
import Data.Functor.Identity (Identity(..))
import Optics (modifying, makeFieldLabelsNoPrefix)
import Optics
import Prettyprinter
import Control.Monad.State.Class
import Data.Kind (Type)

-- import Prettyprinter.Render.Terminal (bgColor, Color (Red), AnsiStyle)

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
-- data SpecialAccumTag = SASum | SANegate | SAProduct | SAPack | SAUnpack deriving (TH.Lift, Eq, Ord)
-- instance Show SpecialAccumTag where
--     show SASum     = "+"
--     show SANegate  = "-"
--     show SAProduct = "*"
--     show SAPack    = "@"
--     show SAUnpack  = "%"

-- Pattern variable tags, holding the origin-type of the pattern variable and any special data it needs to operate
data PVarTag = PVarNothingSpecial | PVarRegexGroup deriving (TH.Lift, Eq, Ord)

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

-- | Given a PVar, what sequence of sigils is it spelled with?
pvarSigil :: PVar -> T.Text
pvarSigil (PVar eager tag _) = T.pack $ go tag : (if eager then "!" else "")
    where
        go PVarNothingSpecial = ':'
        -- go (PVarSpecialAccum _) = '?'
        go PVarRegexGroup = '$'

-- | What name do we use for PVars that go into the Binder? 
pvarBinderName :: PVar -> T.Text
pvarBinderName pvar = T.cons (T.head $ pvarSigil pvar) (pvarName pvar)

showPVar :: PVar -> T.Text
showPVar pvar = T.concat [pvarSigil pvar, pvarName pvar]
instance Show PVar where 
    show = T.unpack . showPVar

-- Runtime values. This is the structure that Rosin trees are parameterized over: the "leaf type".
data RValue = RSymbol T.Text | RString T.Text | RRegex ICU.Regex | RNumber Integer | RVariable PVar deriving (TH.Lift, Eq, Ord)
instance Show RValue where
    show :: RValue -> String
    show (RSymbol t) = T.unpack t
    show (RString t) = T.unpack . T.concat $ ["\"", t, "\""]
    show (RRegex t) = T.unpack . T.concat $ ["/", ICU.pattern t, "/"]
    show (RNumber t) = show t
    show (RVariable t) = show t

instance Pretty RValue where
    pretty = viaShow

-- The tree!
type TagType = Int -- using a type synonym just in case this ever gets extended
defaultTag :: TagType
defaultTag = -1 -- the default tag to use when we don't care about tagging (i.e. grabbing templates or parsing)
data Tree a = Branch !TagType [Tree a] | Leaf !TagType a deriving (TH.Lift, Functor, Foldable, Traversable, Ord)

instance Eq a => Eq (Tree a) where
    (==) (Branch _ ts1) (Branch _ ts2) = ts1 == ts2
    (==) (Leaf _ tip1) (Leaf _ tip2) = tip1 == tip2
    (==) _ _ = False

-- can't make this a Pretty instance because it's a synonym
prettyTag :: TagType -> Doc ann
prettyTag t = if defaultTag == t then "" else ":" <> pretty t

instance Pretty a => Pretty (Tree a) where
    pretty :: Pretty a => Tree a -> Doc ann
    pretty (Branch t forest) = vsep [nest 2 $ vsep ["(" <> prettyTag t, hsep $ forest <&> pretty], ")" <> prettyTag t]
    pretty (Leaf t tip) =  pretty tip <> prettyTag t

prettyTreeWithFocus :: Eq a => Pretty a => ann -> Tree a -> Tree a -> Doc ann
prettyTreeWithFocus ann focus tree = let highlighter = if tree == focus then annotate ann else id
    in highlighter $ case tree of 
        Branch t forest -> vsep [nest 2 $ vsep ["(" <> prettyTag t, hsep $ forest <&> prettyTreeWithFocus ann focus], ")" <> prettyTag t]
        Leaf t tip -> pretty tip <> prettyTag t

pattern LeafSym :: T.Text -> Tree RValue
pattern LeafSym sym <- Leaf _ (RSymbol sym)

pattern LeafStr :: T.Text -> Tree RValue
pattern LeafStr sym <- Leaf _ (RString sym)

-- | allTags applies a predicate to all tags and returns true if all fit
allTags :: (TagType -> Bool) -> Tree a -> Bool
allTags f (Branch ourTag forest) = f ourTag && all (allTags f) forest 
allTags f leaf = f (leaf ^. tag)


-- | tagAll applies a tag to every node in the tree
tagAll :: TagType -> Tree a -> Tree a
tagAll tag' (Branch _ forest) = Branch tag' $ tagAll tag' <$> forest
tagAll tag' leaf = (tag .~ tag') leaf


tag :: Lens (Tree a) (Tree a) TagType TagType
tag = lens get set
    where
        get (Branch tag _) = tag
        get (Leaf tag _) = tag
        set (Branch _ forest) tag' = Branch tag' forest
        set (Leaf _ tip) tag' = Leaf tag' tip


-- Unwrap one level of branching, if it's possible
unbranch :: Tree a -> [Tree a]
unbranch (Branch _ trees) = trees
unbranch leaf = [leaf]

-- Rewraps a list of trees into one tree, dropping unnecesary nesting
rebranch :: TagType -> [Tree RValue] -> Tree RValue
rebranch _ [t] = t
rebranch tag ts = Branch tag ts

-- sexprprint :: Show a => Tree a -> String
sexprprint (Leaf tag a) = show a
sexprprint (Branch tag as) = "(" ++ unwords (map sexprprint as) ++ ")"

instance (Show a) => Show (Tree a) where
    show = sexprprint


-- | The Binder type, holding the intermediate state needed to apply a single rewrite rule.
data Binder = Binder {
    treeBindings :: M.Map T.Text (Tree RValue),
    regexBindings :: M.Map T.Text T.Text
}
makeFieldLabelsNoPrefix ''Binder
makeClassy ''Binder

-- | The MonadBinder constraint: can this transformer stack access a Binder?
-- If so, you can `use binder` (generated by makeClassy), or `use #binder` (generated by makeFieldLabelsNoPrefix)
class (MonadState b m, HasBinder b) => MonadBinder b (m :: Type -> Type) where
instance (MonadState b m, HasBinder b) => MonadBinder b m where

runBinderT :: StateT Binder m a -> Binder -> m (a, Binder)
runBinderT = runStateT
runBinder :: StateT Binder Identity a -> Binder -> (a, Binder)
runBinder st = runIdentity . runStateT st

emptyBinder :: Binder
emptyBinder = Binder {
    treeBindings = mempty,
    regexBindings = mempty
}

-- | Use the lens l on the current state, and run a function under that context
-- Is the same as doing `use l >>= pure . f` as far as I can tell
uses :: (MonadState s m, Is k A_Getter) => Optic' k is s a1 -> (a1 -> a2) -> m a2
uses l f = gets (views l f)

-- | Bind a new or existing tree pattern variable. If the variable is already bound, tack onto its binding list.
-- addTreeBinding :: Monad m => PVar -> Tree RValue -> BinderT m ()
-- addTreeBinding pvar binding = modifying #treeBindings (M.alter go (pvarBinderName pvar))
--     where
--         go Nothing = Just [binding]
--         go (Just existingBindings) = Just $ binding:existingBindings

-- overwriteTreeBinding :: Monad m => PVar -> Tree RValue -> BinderT m ()
-- overwriteTreeBinding pvar binding = modifying #treeBindings (M.insert (pvarBinderName pvar) [binding])

-- | Get the binding list for a tree pattern variable
-- getTreeBinding :: (MonadState s m, HasBinder s) => PVar -> m (Maybe [Tree RValue])
getTreeBinding :: MonadBinder b m => PVar -> m (Maybe (Tree RValue))
getTreeBinding pvar = uses (binder % #treeBindings) (M.lookup (pvarBinderName pvar))

-- | Overwrite a new or existing tree pattern variable.
-- Returns true if we bound a new variable or we didn't change the old binding.
setTreeBinding :: MonadBinder b m => PVar -> Tree RValue -> m Bool
setTreeBinding pvar bdg = do
    prevBinding <- getTreeBinding pvar
    let success = case prevBinding of
            Just bdg_ -> bdg == bdg_
            _ -> True
    modifying (binder % #treeBindings) (M.insert (pvarBinderName pvar) bdg)
    pure success

-- | Bind a new tree pattern variable, or modify an existing with a function.
-- setOrModifyTreeBinding :: MonadBinder b m => PVar -> Tree RValue -> -> m ()
-- setOrModifyTreeBinding pvar bdg = do
--     prevBinding <- getTreeBinding pvar
--     case prevBinding of
--         Just bdg_ -> bdg == bdg_
--         _ -> True
--     modifying #treeBindings (M.insert (pvarBinderName pvar) bdg)
--     pure success


-- | Bind a new or existing regex match variable. Expects a groupname with no sigil attached.
-- Overwrites previously bound variables.
addRegexBinding :: MonadBinder b m => T.Text -> T.Text -> m ()
addRegexBinding groupname matchtext = modifying (binder % #regexBindings) $ M.insert groupname matchtext

-- | Get the binding list for a tree pattern variable
getRegexBinding :: MonadBinder b m => PVar -> m (Maybe T.Text)
getRegexBinding pvar = uses (binder % #regexBindings) (M.lookup (pvarName pvar))

-- | Get the correct binding for a PVar. Some PVars may be stored differently in the Binder depending on their tag.
getBinding :: MonadBinder b m => PVar -> m (Maybe (Tree RValue))
getBinding pvar = case pvarTag pvar of
    PVarRegexGroup -> fmap (Leaf defaultTag . RString) <$> getRegexBinding pvar
    _ -> getTreeBinding pvar

-- Flatten a bunch of RValue trees into one Tree Branch in DFS order
deepFlatten :: [Tree a] -> Tree a
deepFlatten = Branch defaultTag . go
    where go [] = []
          go [end] = [end]
          go (val:(Leaf tag end):_) = [val, Leaf tag end]
          go (val:(Branch _ rest):_) = val : go rest

-- | ‧͙⁺˚*･༓☾ Try to match a single pattern at the tip of a tree ☽༓･*˚⁺‧͙ --
-- Statefully return the variables bound on a successful application --
tryApply :: MonadBinder b m
         => (Tree RValue -> Bool)                   -- Submatcher: does a given tree match anything else? Used for delayed variables
         -> Tree RValue                             -- Input tree
         -> Tree RValue                             -- Pattern to match
         -> m Bool                                  -- Updated variable bindings, along with whether the match succeeded
-- Match pattern variables
tryApply submatcher rval (Leaf _ (RVariable pvar)) = 
    -- Check for submatches and fail out if we're matching an eager variable 
    if pvarEager pvar && submatcher rval then pure False else setTreeBinding pvar rval
-- Match ntree branch patterns exactly
tryApply _ (Branch _ []) (Branch _ []) = pure True
tryApply _ (Branch _ []) _ = pure False
tryApply rules (Branch _ rtrees) (Branch _ pvals)
    -- No point to checking patterns that match branches of the wrong length
    | length rtrees /= length pvals = pure False
    | otherwise = and <$> zipWithM (tryApply rules) rtrees pvals
-- Match symbol patterns
tryApply _ rval pleaf@(Leaf _ (RSymbol _)) = pure $ rval == pleaf
-- Match number patterns
tryApply _ rval pleaf@(Leaf _ (RNumber _)) = pure $ rval == pleaf
-- Match string patterns 
tryApply _ rval pleaf@(Leaf _ (RString _)) = pure $ rval == pleaf
-- Match regex, setting variables corresponding to all captures
tryApply _ (Leaf _ (RString rstr)) (Leaf _ (RRegex preg)) =
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


-- | Apply variable bindings to a pattern, "filling it out" and modifying any pattern variables according to the binding.
betaReduce :: MonadBinder b m => Tree RValue -> m (Tree RValue)
betaReduce (Branch tag trees) = Branch tag <$> mapM betaReduce trees -- Recursive case
betaReduce input@(Leaf tag (RVariable pvar)) = fromMaybe input <$> getBinding pvar -- Base case, matching a pattern var
-- Perform regex capture group substitutions!
betaReduce (Leaf tag (RString pstr)) = do
    translations <- uses (binder % #regexBindings) (fmap (first (T.cons '$')) . M.toList)
    pure . Leaf tag . RString $ foldr (uncurry T.replace) pstr translations
betaReduce (Leaf tag pval) = pure $ Leaf tag pval