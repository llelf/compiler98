module Prelude where

import DPrelude
mycPutChar primitive 1 :: Char -> Either IOError ()

_myhPutChar sr t = fun2 mpc sr t
    where mpc t h (R c _) = let v = mycPutChar c in myseq v (R v t)
