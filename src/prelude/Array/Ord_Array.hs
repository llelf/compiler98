module Array(Ord(..)) where

import DArray
import Assocs
import Eq_Array

instance  (Ix a, Ord a, Ord b) => Ord (Array a b)  where
    a <=  a'            =  assocs a <=  assocs a'
