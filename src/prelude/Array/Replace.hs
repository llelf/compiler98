module Array where

import DArray
import ArrayFun
import Indices
import Difference
import Index
import Bounds

infixl 9  //

(//)                  :: (Ix a) => Array a b -> [(a,b)] -> Array a b
a // us               =  array (bounds a)
                            ([(i,a!i) | i <- indices a \\ [i | (i,_) <- us]]
                             ++ us)
