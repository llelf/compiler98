module Prelude({-cHClose,-}primHClose,Handle) where

import PreludeBuiltin(Handle)
import DIO

--cHClose primitive 1 :: Handle -> ()

primHClose                :: Handle -> IO () 
primHClose h = 
  IO ( \ world -> case _hClose h of
		    () -> Right ())


_hClose h = _prim _tprim_HClose h

_tprim_HClose primitive 2 :: Trace -> R Handle -> R ()
