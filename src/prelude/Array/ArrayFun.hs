module Array where

#if 0
--  !defined(TRACING)

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

-- The following definitely works!

import DArray
import Ix
import IOExtras

array :: (Ix a) => (a,a) -> [(a,b)] -> Array a b
array b ivs =
    if and [inRange b i | (i,_) <- ivs]
    then unsafePerformIO ( do
           a <- newIOArray b (error "Array.!: undefined array element")
           mapM_ (\(i,v)-> writeIOArray a i v) ivs
           freezeIOArray a
         )
    else error "Array.array: out-of-range array association"

#else

import DArray
import Ix
import LowVector
import _E

array :: (Ix a) => (a,a) -> [(a,b)] -> Array a b
array b ivs =
    if and [inRange b i | (i,_) <- ivs]
    then unsafePerformIO ( do
           v <- primNewVectorC (rangeSize b)
                               (_E (error "Array.!: undefined array element"))
           mapM_ (\(i,a)-> primUpdateVectorC (index b i) (_E a) v) ivs
           return (MkArray b v)
         )
    else error "Array.array: out-of-range array association"

#endif
