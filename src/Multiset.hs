module Multiset where
import qualified Data.Map as M


newtype Ord a => Multiset a = Multiset { unmultiset :: M.Map a Int } deriving (Show)

-- Is this element inside the multiset in a sufficient quantity? Just the new multiset if so
check :: Ord a => (a, Int) -> Multiset a -> Maybe (Multiset a)
check (x, n) = maybe False (n <=) . M.lookup x . unmultiset 

