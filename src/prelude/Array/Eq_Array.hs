module Array(Eq(..)) where

import DArray
--import Assocs
import Bounds
import Elems

instance  (Ix a, Eq a, Eq b)  => Eq (Array a b)  where
 -- a == a'             =  assocs a == assocs a'
    a == a'             =  bounds a == bounds a' && elems a == elems a'
