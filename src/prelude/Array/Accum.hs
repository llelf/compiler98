module Array where

import DArray
import Replace
import Index

accum                 :: (Ix a) => (b -> c -> b) -> Array a b -> [(a,c)]
                                   -> Array a b
accum f               =  foldl (\a (i,v) -> a // [(i,f (a!i) v)])
