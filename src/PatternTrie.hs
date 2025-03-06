-- Please visit the original pattern-trie library at https://gitlab.com/romanb/pattern-trie/-/tree/master?ref_type=heads
-- 
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE
    BangPatterns
  , DeriveGeneric
  , DeriveAnyClass
  , DerivingStrategies
  , TupleSections
#-}

-- | A variant of a (radix) trie with the following characteristics:
--
--   * Keys are simple 'Pattern's composed of 'Matcher's and hence a single key
--     can match multiple input 'Str'ings.
--   * Looking up a 'match' for a 'Str'ing can 'Capture' parts of the string.
--   * Both patterns  and input strings are understood as being composed of
--     (indivisible) chunks of strings of a generic type @s@ (typically
--     instantiated to either 'Text' or 'ByteString'). More precisely, every
--     chunk of an input 'Str'ing is tested against a 'Matcher' of a 'Pattern'
--     in sequence. As a result, pattern tries usually end up less compact than
--     more general tries, since sharing of prefixes as well as all operations
--     are limited to the granularity of these chunks.
--
-- These characteristics hint at the primary intended use-case, whereby
-- keys have a \"natural\" decomposition into chunks and the same
-- chunks are heavily shared by different keys, e.g. as in directory trees.
-- A pattern trie allows to associate values with simple patterns, whereby a
-- single value can essentially be looked up by all strings matching a pattern,
-- thereby capturing parts of it.
--
-- __Strictness:__
-- A 'Trie' is strict in the spine as well as the values (WHNF).
--
-- __Ordering:__ The order of keys and thus elements is unspecified.
-- No particular order may be assumed by folds and traversals, whose
-- combining operations should hence be commutative.
--
-- __Example:__
--
-- >>> :set -XOverloadedStrings
--
-- >>> import Data.ByteString (ByteString)
--
-- >>> let p1 = mempty |> EqStr "home" |> EqStr "alice" |> EqStr "tmp"
-- >>> let p2 = mempty |> EqStr "home" |> AnyStr        |> EqStr "music"
-- >>> let p3 = mempty |> EqStr "data" |> EqStr "bob"   |> EqStr "books"
-- >>> let p4 = mempty |> EqStr "data" |> AnyStr        |> EqStr "books"
-- >>> let p5 = mempty |> EqStr "data" |> AnyStr        |> EqStr "books" |> EqStr "sicp"
--
-- >>> let trie = fromAssocList $ [p1,p2,p3,p4,p5] `zip` [1..] :: Trie ByteString Int
--
-- >>> match ["home","alice","tmp"] trie
-- Just (1,fromList [])
--
-- >>> match ["home","bob","tmp"] trie
-- Nothing
--
-- >>> match ["home","alice","music"] trie
-- Just (2,fromList [Capture {captured = "alice"}])
--
-- >>> match ["home","bob","music"] trie
-- Just (2,fromList [Capture {captured = "bob"}])
--
-- >>> match ["data","bob","books"] trie
-- Just (3,fromList [])
--
-- >>> match ["data","alice","books"] trie
-- Just (4,fromList [Capture {captured = "alice"}])
--
-- >>> match ["data","alice","books","sicp"] trie
-- Just (5,fromList [Capture {captured = "alice"}])
--
-- >>> match ["data","bob","books","sicp"] trie
-- Just (5,fromList [Capture {captured = "bob"}])
--
-- >>> matchPrefix ["data","alice","books","wonderland"] trie
-- Just (4,fromList [Capture {captured = "alice"}],["wonderland"])
--
-- >>> matchPrefix ["data","bob","books","wonderland"] trie
-- Just (4,fromList [Capture {captured = "bob"}],["wonderland"])
--
-- >>> let (t,c,s) = matchPrefixTrie ["data","bob","books","wonderland"] trie
-- >>> (value t, c, s)
-- (Just 4,fromList [Capture {captured = "bob"}],["wonderland"])
--
module PatternTrie
    ( Trie, value

    -- * Patterns
    -- $definitions
    , Pattern, Str, Matcher (..), Capture (..)

    -- ** Testing Patterns
    , overlapping
    , MatchOrd (..), MatchPrefixOrd (..)
    , matchOrd, matchPrefixOrd
    , apply
    , applyCapture
    , unapplyCapture
    , applyMatch
    , applyMatches
    , applyMatchPrefix
    , applyMatchesPrefix

    -- * List conversion
    , fromAssocList
    , toAssocList

    -- * Modifications
    , insert
    , adjust
    , delete

    -- * 'Pattern' lookup
    -- $lookupBigO
    , lookup
    , lookupPrefix
    , lookupPrefixTrie

    -- * 'Str'ing matching
    -- $matchingBigO
    , match
    , matchPrefix
    , matchPrefixTrie

    -- * Special folds and traversals
    , traverseWithKey
    , foldMapWithKey
    , foldrWithKey

    -- * Re-exports
    , (|>)
    ) where

import GHC.Generics (Generic)

import Control.Applicative
import Control.Monad ((<$!>))
import Data.Coerce (coerce)
import Data.Foldable
import Data.Hashable
import Data.List (foldl')
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe, isJust)
import Data.Semigroup
import Data.Sequence (Seq (..), (|>))
import Prelude hiding (lookup)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence       as Seq
import qualified Data.Traversable    as Traversable

-- {-# ANN module "HLint: ignore Eta reduce" #-}

-- | An unordered map from 'Pattern's of strings of type @s@ to values
-- of type @a@.
data Trie s a = Trie
    { strtries :: !(HashMap s (Trie s a))
    , vartrie  :: !(Maybe (Trie s a))
    , value    :: !(Maybe a)
        -- ^ The value at the root of the trie, i.e.
        --
        -- @
        -- value t == 'lookup' mempty t
        -- @
    } deriving stock (Eq, Show, Read, Generic)

instance Traversable (Trie s) where
    traverse f = traverseWithKey (const f)

instance Functor (Trie s) where
    fmap = Traversable.fmapDefault

instance Foldable (Trie s) where
  foldMap = Traversable.foldMapDefault

  null (Trie a Nothing Nothing) = HashMap.null a
  null _                        = False
  {-# INLINE null #-}

-- | /Note (left preference)/: If two tries have a value attached to
-- the same 'Pattern' (i.e. to the same key), then @t1 <> t2@ preserves
-- the value of @t1@.
instance (Eq s, Hashable s) => Semigroup (Trie s a) where
    a <> b =  Trie (HashMap.unionWith (<>) (strtries a) (strtries b))
                   (vartrie a <> vartrie b)
                   (value a <|> value b)

    stimes = stimesIdempotent

-- | /Note/: @mappend = (<>)@.
instance (Eq s, Hashable s) => Monoid (Trie s a) where
    mempty  = Trie mempty Nothing Nothing
    mappend = (<>)

-----------------------------------------------------------------------------
-- Patterns

-- $definitions
-- __Definition (/Prefix Match/):__ A 'Str'ing is a /prefix match/ for a
-- 'Pattern', iff all 'Matcher's in the pattern succeed when applied on
-- the chunks of the input string in sequence. A /proper prefix match/
-- is a prefix match that is not a (full) match.
--
--  A prefix match is witnessed by 'applyMatchesPrefix'.
--
-- __Definition (/(Full) Match/):__ A 'Str'ing is a /(full) match/ for a
-- 'Pattern', iff it is a prefix match and there are no unmatched
-- remaining chunks of the input (i.e. the string and the pattern have
-- the same length).
--
--  A (full) match is witnessed by 'applyMatches'.
--
-- __Definition (/Overlapping Patterns/):__
-- Two patterns are /overlapping/, iff they are not equal and
-- there exists an input 'Str'ing that is a (full) match for both patterns.
--
-- Overlapping patterns are witnessed by 'overlapping'.

-- | A pattern is a sequence of 'Matcher's and serves as a key in a pattern
-- trie.
--
-- If two patterns are overlapping for an input string, the preference for
-- a 'match' is given by the partial order @EqStr > AnyStr@ on the competing
-- matchers, i.e. towards the more specific pattern. This partial order is
-- witnessed and subsumed by the total order 'MatchOrd'.
--
-- The preference for a prefix match is reversed, i.e. for an input string where
-- only a proper prefix is a match for overlapping patterns, the preference
-- is given by the partial order @AnyStr > EqStr@, i.e. towards the more general
-- pattern. This partial order is witnessed and subsumed by the total order
-- 'PrefixMatchOrd'.
type Pattern s = Seq (Matcher s)

-- | A (chunked) input string to 'match' on a 'Pattern' in a trie.
--
-- /Note:/ Input strings can be infinite. Since the tries are always finite,
-- an infinite input string is only consumed until either a match has been
-- found or the applicable paths in the trie have been exhaustively searched.
type Str s = [s]

-- | A captured chunk of an input 'Str'ing.
newtype Capture s = Capture { captured :: s }
    deriving stock (Eq, Ord, Show, Read, Generic)

-- | A 'Matcher' is applied on a single chunk of an input 'Str'ing
-- while looking for a 'match' and either /succeeds/ or /fails/. If it succeeds,
-- it may 'Capture' the chunk.
data Matcher s
    -- | Match and capture an arbitrary chunk of an input string.
    = AnyStr
    -- | Match a chunk of an input string exactly, capturing nothing.
    | EqStr !s
    deriving stock (Eq, Show, Read, Generic)

-- | A total order for matchers that subsumes the partial order for
-- the preference between overlapping patterns on a 'matchPrefix'.
--
-- >>> MatchPrefixOrd AnyStr > MatchPrefixOrd (EqStr "a")
-- True
--
-- >>> let p1 = mempty |> EqStr "a" |> EqStr "b"
-- >>> let p2 = mempty |> AnyStr    |> EqStr "b"
-- >>> matchPrefixOrd p1 < matchPrefixOrd p2
-- True
newtype MatchPrefixOrd s = MatchPrefixOrd (Matcher s)
    deriving Eq

matchPrefixOrd :: Pattern s -> Seq (MatchPrefixOrd s)
matchPrefixOrd = coerce

-- | A total order for matchers that subsumes the partial order for
-- the preference between overlapping patterns on a 'match'.
--
-- >>> MatchOrd AnyStr < MatchOrd (EqStr "a")
-- True
--
-- >>> let p1 = mempty |> EqStr "a" |> EqStr "b"
-- >>> let p2 = mempty |> AnyStr    |> EqStr "b"
-- >>> matchOrd p1 > matchOrd p2
-- True
newtype MatchOrd s = MatchOrd (Matcher s)
    deriving Eq

matchOrd :: Pattern s -> Seq (MatchOrd s)
matchOrd = coerce

instance Ord s => Ord (MatchOrd s) where
    compare (MatchOrd m1) (MatchOrd m2) =
        compare (MatchPrefixOrd m2) (MatchPrefixOrd m1)

instance Ord s => Ord (MatchPrefixOrd s) where
    compare (MatchPrefixOrd m1) (MatchPrefixOrd m2) =
        case (m1, m2) of
            (AnyStr  ,  EqStr _) -> GT
            (EqStr  _,   AnyStr) -> LT
            (AnyStr  ,   AnyStr) -> EQ
            (EqStr s1, EqStr s2) -> compare s1 s2

-- | Check whether two patterns are overlapping, i.e. whether there
-- exists a 'Str'ing that is a (full) match for both patterns.
--
-- >>> let p1 = mempty |> EqStr "a" |> AnyStr
-- >>> let p2 = mempty |> AnyStr    |> EqStr "b"
-- >>> let p3 = mempty |> EqStr "a" |> EqStr "c"
-- >>> overlapping p1 p1
-- False
-- >>> overlapping p1 p2
-- True
-- >>> overlapping p1 p3
-- True
-- >>> overlapping p2 p3
-- False
overlapping :: Eq s => Pattern s -> Pattern s -> Bool
overlapping p1 p2 = Seq.length p1 == Seq.length p2 && go True p1 p2
  where
    go eq Empty             Empty              = not eq
    go _  Empty             _                  = False
    go _  _                 Empty              = False
    go _  (EqStr _ :<| p1') (AnyStr   :<| p2') = go False p1' p2'
    go _  (AnyStr  :<| p1') (EqStr _  :<| p2') = go False p1' p2'
    go eq (AnyStr  :<| p1') (AnyStr   :<| p2') = go eq    p1' p2'
    go eq (EqStr s :<| p1') (EqStr s' :<| p2') = s == s' && go eq p1' p2'

-- | Apply a string to a pattern, returning the unmatched
-- suffix of the pattern together with the captured chunks and the
-- remaining (unmatched) suffix of the input string.
--
-- >>> let p = mempty |> EqStr "a" |> AnyStr |> EqStr "c"
-- >>> let s = ["a", "b", "d"]
-- >>> apply s p
-- (fromList [EqStr "c"],fromList [Capture {captured = "b"}],["d"])
apply :: Eq s => Str s -> Pattern s -> (Pattern s, Seq (Capture s), Str s)
apply = go Seq.empty
  where
    go !cs ss Empty = (Empty, cs, ss)
    go !cs [] p     = (p    , cs, [])
    go !cs str@(s:s') pat@(m :<| p') = case m of
        AnyStr   -> go (cs |> Capture s) s' p'
        EqStr ss -> if s == ss
            then go cs s' p'
            else (pat, cs, str)

-- | Apply a string to a pattern, returning the captures.
--
-- >>> let p = mempty |> EqStr "a" |> AnyStr |> EqStr "c"
-- >>> let s = ["a", "b", "d"]
-- >>> applyCapture s p
-- fromList [Capture {captured = "b"}]
applyCapture :: Eq s => Str s -> Pattern s -> Seq (Capture s)
applyCapture s p = case apply s p of
    (_, c, _) -> c
{-# INLINE applyCapture #-}

-- | (Re)Construct the longest input 'Str'ing matching a prefix of a pattern,
-- using the given captures to satisfy matchers.  As long as there are enough
-- captures to satisfy all matchers in the pattern, the resulting string will
-- always be a (full) match for the pattern.
--
-- Furthermore, if an input string @s@ is a (full) match for a pattern @p@, then
--
-- @
-- unapplyCapture p (applyCapture s p) == s
-- @
--
-- >>> let p = mempty |> EqStr "a" |> AnyStr |> EqStr "c"
-- >>> let s = ["a", "b", "c"]
-- >>> unapplyCapture p (applyCapture s p)
-- ["a","b","c"]
unapplyCapture :: Pattern s -> Seq (Capture s) -> Str s
unapplyCapture = go []
  where
    go !str Empty           _          = str
    go !str (_ :|> AnyStr ) Empty      = str
    go !str (p :|> EqStr s) cs         = go (s : str) p cs
    go !str (p :|> AnyStr ) (cs :|> c) = go (captured c : str) p cs

-- | Apply a string to a pattern, returning the captures iff
-- the string is a (full) match for the pattern.
--
-- >>> let p = mempty |> EqStr "a" |> AnyStr |> EqStr "c"
-- >>> applyMatch ["a", "b", "c", "d"] p
-- Nothing
-- >>> applyMatch ["a", "b", "c"] p
-- Just (fromList [Capture {captured = "b"}])
applyMatch :: Eq s => Str s -> Pattern s -> Maybe (Seq (Capture s))
applyMatch s p = case apply s p of
    (Empty, cs, []) -> Just cs
    _               -> Nothing

-- | Apply a string to a pattern, returning 'True' iff the string
-- is a (full) match for the pattern.
applyMatches :: Eq s => Str s -> Pattern s -> Bool
applyMatches s = isJust . applyMatch s

-- | Apply a string to a pattern, returning the captures iff
-- the string is a prefix match for the pattern.
--
-- >>> let p = mempty |> EqStr "a" |> AnyStr |> EqStr "c"
-- >>> applyMatchPrefix ["a", "b", "c", "d"] p
-- Just (fromList [Capture {captured = "b"}])
applyMatchPrefix :: Eq s => Str s -> Pattern s -> Maybe (Seq (Capture s))
applyMatchPrefix s p = case apply s p of
    (Empty, cs, _) -> Just cs
    _              -> Nothing

-- | Apply a string to a pattern, returning 'True' iff the string
-- is a prefix match for the pattern.
applyMatchesPrefix:: Eq s => Str s -> Pattern s -> Bool
applyMatchesPrefix s = isJust . applyMatchPrefix s

-----------------------------------------------------------------------------
-- List conversion

-- | Create a pattern trie from a list of patterns and associated values.
--
-- \(\mathcal{O}(n \cdot k)\), where \(n\) is the length of the list and
-- \(k\) is the length of the longest pattern in the list.
fromAssocList :: (Eq s, Hashable s) => [(Pattern s, a)] -> Trie s a
fromAssocList = foldl' add mempty
  where
    add t (p, a) = insert p a t
{-# INLINE fromAssocList #-}

-- | Create a list of patterns and associated values from a pattern trie.
--
-- \(\mathcal{O}(n \cdot k)\), where \(n\) is the number of values in the trie
-- and \(k\) is the length of the longest pattern in the trie.
toAssocList :: (Eq s, Hashable s) => Trie s a -> [(Pattern s, a)]
toAssocList t = foldrWithKey (\p a l -> (p, a) : l) [] t
{-# INLINE toAssocList #-}

-----------------------------------------------------------------------------
-- Updates

-- | Insert the value for the given pattern into the trie.
--
-- \(\Theta(k)\), where \(k\) is the length of the pattern.
insert :: (Eq s, Hashable s) => Pattern s -> a -> Trie s a -> Trie s a
insert p !a = go p
  where
    go Empty            = modVal (const (Just a))
    go (AnyStr  :<| p') = modVar ((Just $!) . go p' . fromMaybe mempty)
    go (EqStr s :<| p') = modStr $ \m ->
        let t' = HashMap.lookupDefault mempty s m
        in HashMap.insert s (go p' t') m

-- | Update the value of the given pattern in the trie, if it exists.
--
-- \(\mathcal{O}(k)\), where \(k\) is the length of the pattern.
adjust :: (Eq s, Hashable s) => Pattern s -> (a -> a) -> Trie s a -> Trie s a
adjust p f = go p
  where
    go Empty            = modVal (f <$!>)
    go (AnyStr  :<| p') = modVar (go p' <$!>)
    go (EqStr s :<| p') = modStr (HashMap.adjust (go p') s)

-- | Remove the value for the given pattern from the trie, if it exists.
--
-- \(\mathcal{O}(k)\), where \(k\) is the length of the pattern.
delete :: (Eq s, Hashable s) => Pattern s -> Trie s a -> Trie s a
delete p = go p
  where
    go Empty            = modVal (const Nothing)
    go (AnyStr  :<| p') = modVar (maybe Nothing (go' p'))
    go (EqStr s :<| p') = modStr (HashMap.update (go' p') s)

    go' p' t = case go p' t of
        t' | null t' -> Nothing
        t'           -> Just t'

-----------------------------------------------------------------------------
-- Lookups

-- $lookupBigO
-- ===Time Complexity (successful lookups)
-- \(\Theta(k)\), where \(k\) is the length of the pattern.

type LookupNextR r s a = Trie s a -> Pattern s -> r -> r

lookupIter :: (Eq s, Hashable s) => LookupNextR r s a -> r -> Pattern s -> Trie s a -> r
lookupIter nextR = go
  where
    go r p t =
        let !r' = nextR t p r
        in case p of
            Empty          -> r'
            AnyStr  :<| p' -> maybe r' (go r' p') (vartrie t)
            EqStr s :<| p' -> maybe r' (go r' p') (HashMap.lookup s (strtries t))
{-# INLINE lookupIter #-}

-- | Lookup the trie rooted at the longest prefix of a pattern, returning it
-- together with the remaining suffix of the pattern.
lookupPrefixTrie :: (Eq s, Hashable s) => Pattern s -> Trie s a -> (Trie s a, Pattern s)
lookupPrefixTrie p t = lookupIter nextR (t, Empty) p t
  where
    nextR t' p' = const (t', p')

-- | Lookup the value for the longest matching prefix of a pattern,
-- returning it together with the remaining suffix of the pattern.
-- If there is no value in the trie for any prefix of the given pattern,
-- the result is 'Nothing'.
lookupPrefix :: (Eq s, Hashable s) => Pattern s -> Trie s a -> Maybe (a, Pattern s)
lookupPrefix p t = lookupIter nextR Nothing p t
  where
    nextR t' p' r = ((,p') <$!> value t') <|> r

-- | Lookup the value of a pattern.
-- If there is no value in the trie for the given pattern, the result is
-- 'Nothing'.
lookup :: (Eq s, Hashable s) => Pattern s -> Trie s a -> Maybe a
lookup p t = case lookupPrefixTrie p t of
    (t', Empty) -> value t'
    _           -> Nothing
{-# INLINE lookup #-}

-----------------------------------------------------------------------------
-- Matching

-- $matchingBigO
-- ===Time Complexity (successful matches)
-- In what follows \(k\) is always the length of the input string (i.e. the
-- number of chunks).
--
-- __/Best case/__: \(\Theta(k)\), when the input string matches the most
-- specific pattern in the trie (i.e. with the least captures) from all those
-- that have a matching prefix for the string.
--
-- __/Worst case/__: \(\mathcal{O}(2^k)\), when there are \(2^{k-1}\) distinct
-- patterns of length at least \(k\) in the trie, all of which have a prefix of
-- length \(k-1\) that is a prefix match for the input string, but none except
-- for the most general of them are an actual (full) match. This is a
-- pathological case that comes about from backtracking to more general patterns
-- and is illustrated in an example with \(k=3\) for the input string
-- @["a","a","b"]@ below.
--
-- <<docs-extra/backtrack.png>>
--
-- Nodes with values are filled, choice points are blue, dead ends are red and
-- dashed lines indicate backtracking.  The above trie contains the keys
-- (patterns)
--
--   * @mempty |> EqStr "a" |> EqStr "a" |> EqStr "a"@
--   * @mempty |> EqStr "a" |> AnyStr    |> EqStr "a"@
--   * @mempty |> AnyStr    |> EqStr "a" |> EqStr "a"@
--   * @mempty |> AnyStr    |> AnyStr    |> EqStr "b"@
--
-- with some arbitrary values. The paths are explored left-to-right in a
-- depth-first search. The number of steps for a match in the worst case is more
-- accurately approximated by
-- \[ \underbrace{2^k - 1}_{\text{downwards searching}} + \underbrace{2^{k-1} - 1}_{\text{upwards backtracking}} \]
-- Dropping the asymptotically insignificant constants and lower terms yields the
-- bound.  For realistic values of \(k\), however, the difference matters.

-- | A choice point for backtracking to alternative branches.
data Choice s a = Choice !(Seq (Capture s)) (Str s) !(Trie s a)

type MatchNextR r s a = Trie s a -> Seq (Capture s) -> Str s -> r -> r

matchIter :: (Eq s, Hashable s) => MatchNextR r s a -> r -> Str s -> Trie s a -> r
matchIter nextR = go Seq.empty []
  where
    go !cs !cps r str t =
        let !r' = nextR t cs str r
        in case str of
            []     -> if isJust (value t) then r' else backtrack r' cps
            (s:s') -> case HashMap.lookup s (strtries t) of
                Just t' -> case vartrie t of
                    Nothing  -> go cs cps r' s' t'
                    Just t'' ->
                        -- Add a new choice point
                        let cps' = Choice (cs |> Capture s) s' t'' : cps
                        in go cs cps' r' s' t'
                Nothing -> case vartrie t of
                    Just t' -> go (cs |> Capture s) cps r' s' t'
                    Nothing -> backtrack r' cps

    -- Continue at the last choice point, if any
    backtrack r [] = r
    backtrack r (Choice cs' ss' t' : cps') = go cs' cps' r ss' t'
{-# INLINE matchIter #-}

-- | Lookup the trie rooted at the longest matching prefix of the input string,
-- returning it together with any captured parts and the remaining (unmatched)
-- suffix of the input string.
--
-- In particular, if the input string is a (full) match for a pattern, the
-- returned trie is the subtrie that is rooted at the associated 'value'.
matchPrefixTrie :: (Eq s, Hashable s) => Str s -> Trie s a -> (Trie s a, Seq (Capture s), Str s)
matchPrefixTrie s t = matchIter nextR (t, Seq.empty, []) s t
  where
    nextR t' cs' s' = const (t', cs', s')

-- | Lookup the value for the longest matching prefix of the input string,
-- returning it together with any captured parts and the remaining
-- (unmatched) suffix of the input string. If no prefix of the input
-- string matches any pattern in the trie, the result is 'Nothing'.
matchPrefix :: (Eq s, Hashable s) => Str s -> Trie s a -> Maybe (a, Seq (Capture s), Str s)
matchPrefix s t = matchIter nextR Nothing s t
  where
    nextR t' cs s' r = ((,cs,s') <$!> value t') <|> r

-- | Lookup the value for an input string by matching it against the patterns of
-- a trie. The value of the matching pattern, if any, is returned together with
-- any captured parts of the input string. If the input string does not match
-- any pattern in the trie, the result is 'Nothing'.
--
match :: (Eq s, Hashable s) => Str s -> Trie s a -> Maybe (a, Seq (Capture s))
match s t = case matchPrefixTrie s t of
    (t', cs, []) -> (,cs) <$!> value t'
    _            -> Nothing
{-# INLINE match #-}

-----------------------------------------------------------------------------
-- Folds and traversals with keys (patterns)

traverseWithKey :: Applicative f => (Pattern s -> a -> f b) -> Trie s a -> f (Trie s b)
traverseWithKey f t = go mempty t
  where
    go !p (Trie vals vars v) =
        let f1 = HashMap.traverseWithKey (\k -> go (p |> EqStr k)) vals
            f2 = traverse (go (p |> AnyStr)) vars
            f3 = traverse (f p) v
        in Trie <$> f1 <*> f2 <*> f3

foldMapWithKey :: Monoid m => (Pattern s -> a -> m) -> Trie s a -> m
foldMapWithKey f = getConst . traverseWithKey (\p -> Const . f p)

foldrWithKey :: (Pattern s -> a -> b -> b) -> b -> Trie s a -> b
foldrWithKey f b t = appEndo (foldMapWithKey (\p -> Endo . f p) t) b

-----------------------------------------------------------------------------
-- Utilities

modStr :: (HashMap s (Trie s a) -> HashMap s (Trie s a)) ->  Trie s a -> Trie s a
modStr f t = t { strtries = f (strtries t) }
{-# INLINE modStr #-}

modVar :: (Maybe (Trie s a) -> Maybe (Trie s a)) -> Trie s a -> Trie s a
modVar f t = t { vartrie = f (vartrie t) }
{-# INLINE modVar #-}

modVal :: (Maybe a -> Maybe a) -> Trie s a -> Trie s a
modVal f t = t { value = f (value t) }
{-# INLINE modVal #-}

-- type BTrie a = Trie ByteString a
-- type BStr = Str ByteString
-- type BCapture = Capture ByteString
-- type BPattern = Pattern ByteString

-- type TTrie a = Trie Text a
-- type TStr = Str Text
-- type TCapture = Capture Text
-- type TPattern = Pattern Text

