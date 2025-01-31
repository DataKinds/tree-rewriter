module Multiset where
import qualified Data.Map as M
import Control.Monad (foldM)


newtype Ord a => Multiset a = Multiset { unmultiset :: M.Map a Int } deriving (Eq)

instance (Ord a, Show a) => Show (Multiset a) where
    show = unwords . map  (\(term,count) -> show term++"#"++show count) . toList

fromList :: Ord a => [(a, Int)] -> Multiset a
fromList = Multiset . M.fromListWith (+)

toList :: Ord a => Multiset a -> [(a, Int)] 
toList = M.toList . unmultiset

empty :: Ord a => Multiset a
empty = Multiset M.empty

null :: Ord a => Multiset a -> Bool
null = M.null . unmultiset

-- Is this element inside the multiset in a sufficient quantity? 
inside :: Ord a => (a, Int) -> Multiset a -> Bool
inside (x, n) = maybe False (n <=) . M.lookup x . unmultiset 

-- Are these elements inside the multiset in a sufficient quantity? 
allInside :: Ord a => Multiset a -> Multiset a -> Bool
allInside xns ms = all (`inside` ms) (toList xns)

-- Take out this many copies of this element from our multiset, if we can!
grab :: Ord a => (a, Int) -> Multiset a -> Maybe (Multiset a)
grab xn@(x,n) ms = if inside xn ms then Just . Multiset . M.adjust (subtract n) x . unmultiset $ ms else Nothing

-- Take out this many copies of these elements from our multiset, if we can!
grabMany :: Ord a => Multiset a -> Multiset a -> Maybe (Multiset a)
grabMany xns ms = Multiset <$> foldM (\m xn -> unmultiset <$> grab xn (Multiset m)) ms' xns'
    where ms' = unmultiset ms; xns' = toList xns

grabManyRaw :: Ord a => Multiset a -> Multiset a -> Multiset a
grabManyRaw xns ms = Multiset $ M.unionWith (-) (unmultiset ms) (unmultiset xns)

-- Put this many copies of these elements into our multiset
putMany :: Ord a => Multiset a -> Multiset a -> Multiset a
putMany xns ms = Multiset $ M.unionWith (+) (unmultiset xns) (unmultiset ms)