module Array where

#if 0

import DArray
import ArrayFun
import Indices
import Difference
import AIndex
import Bounds

infixl 9  //

(//)                  :: (Ix a) => Array a b -> [(a,b)] -> Array a b
a // us               =  array (bounds a)
                            ([(i,a!i) | i <- indices a \\ [i | (i,_) <- us]]
                             ++ us)

#else

import DArray
import Ix
import LowVector
import _E

infixl 9  //

(//)                  :: (Ix a) => Array a b -> [(a,b)] -> Array a b
(MkArray b v) // us   =	unsafePerformIO (do
			  v' <- primCopyVectorC v
			  mapM_ (\(ix,elt)-> primUpdateVectorC (index b ix)
                                                               (_E elt)
                                                               v')
                                us
			  return (MkArray b v')
			)

#endif
