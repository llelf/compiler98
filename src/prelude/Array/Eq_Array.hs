module Array(Eq(..)) where

import DArray
import Assocs

instance  (Ix a, Eq b)  => Eq (Array a b)  where
    a == a'             =  assocs a == assocs a'
