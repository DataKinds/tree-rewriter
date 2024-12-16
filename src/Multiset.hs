module Multiset where
import qualified Data.Map as M
import Control.Monad (foldM)


newtype Ord a => Multiset a = Multiset { unmultiset :: M.Map a Int } deriving (Show)

-- Is this element inside the multiset in a sufficient quantity? 
check :: Ord a => (a, Int) -> Multiset a -> Bool
check (x, n) = maybe False (n <=) . M.lookup x . unmultiset 


-- Are these elements inside the multiset in a sufficient quantity? 
checkMany :: Ord a => [(a, Int)] -> Multiset a -> Bool
checkMany xns ms = all (`check` ms) xns

-- Take out this many copies of this element from our multiset, if we can!
grab :: Ord a => (a, Int) -> Multiset a -> Maybe (Multiset a)
grab xn@(x,n) ms = if check xn ms then Just . Multiset . M.adjust (subtract n) x . unmultiset $ ms else Nothing

-- Take out this many copies of these elements from our multiset, if we can!
grabMany :: Ord a => [(a, Int)] -> Multiset a -> Maybe (Multiset a)
grabMany xns ms = Multiset <$> foldM (\m xn -> unmultiset <$> grab xn (Multiset m)) (unmultiset ms) xns