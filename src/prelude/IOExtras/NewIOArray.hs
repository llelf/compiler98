module IOExtras
  ( newIOArray
  ) where

import Ix
import DIOArray

newIOArray :: Ix ix => (ix,ix) -> elt -> IO (IOArray ix elt)
newIOArray bounds elt =
    mkVector bounds elt >>= return . MkIOArray bounds


-- primVector is used identically for Array and IOArray
primVector primitive 2 :: Int -> [(Int,a)] -> Vector a

mkVector :: (Ix ix) => (ix,ix) -> elt -> IO (Vector elt)
mkVector bounds val =
    let ivs =  [ (index bounds i, val) | i <- range bounds ]
        force [] f = f
        force ((i,v):xs) f = i `seq` force xs f
    in force ivs (return (primVector (rangeSize bounds) ivs))
