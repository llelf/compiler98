module Array where

import DArray
import Ix

primVector primitive 2 :: Int -> [(Int,a)] -> Vector a

array :: (Ix a) => (a,a) -> [(a,b)] -> Array a b
array b ivs =
    if and [inRange b i | (i,_) <- ivs]
        then MkArray b
                     (let ivs' =  [(index b i,v) | (i,v) <- ivs]
                          force [] f = f
                          force ((i,v):xs) f = i `seq` force xs f
                      in force ivs' (primVector (rangeSize b) ivs'))
{-
                     (\j -> case [v | (i,v) <- ivs, i == j] of
                            [v]   -> v
                            []    -> error "Array.!: \
                                           \undefined array element"
                            _     -> error "Array.!: \
                                           \multiply defined array element")
-}
        else error "Array.array: out-of-range array association"
