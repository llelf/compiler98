module List where

import InsertBy

-- stable sorting algorithm

sortBy                  :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp              =  foldr (insertBy cmp) []
