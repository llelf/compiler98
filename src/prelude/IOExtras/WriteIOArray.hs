module IOExtras
  ( writeIOArray
  ) where

import FFI
import Ix
import DIOArray

writeIOArray :: Ix ix => IOArray ix elt -> ix -> elt -> IO ()
writeIOArray (MkIOArray b v) ix elt =
    let i = index b ix
    in
    primUpdateVector i elt v   `seq`   return ()

-- updateVector is currently used only for IOArray, not for Array
primUpdateVector primitive 3 :: Int -> a -> Vector a -> ()
