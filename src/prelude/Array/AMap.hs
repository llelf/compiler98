module Array where

import DArray
import ArrayFun
import Index
import Bounds

--amap                  :: (Ix a) => (b -> c) -> Array a b -> Array a c
--amap f a              =  array b [(i, f (a!i)) | i <- range b]
--                         where b = bounds a

instance (Ix a) => Functor (Array a) where
    fmap f a  =  array b [(i, f (a!i)) | i <- range b]
                 where b = bounds a

