module Array where

import DArray
import Ix

infixl 9  !
primIndex primitive 2 :: Vector a -> Int -> a

(!)                   :: (Ix a) => Array a b -> a -> b
(!) (MkArray b v) i   =  let i' = index b i in i' `seq` primIndex  v i'
