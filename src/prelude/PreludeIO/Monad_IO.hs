module Prelude where

import DIO
import DIOError(IOError(..))   -- problem with cycle [Prelude _DIO]
-- import CMonad

instance Monad IO where
    x >>= y  = IO (primBind1 x y)
	       where 
		primBind1 (IO xf) y w =
		  case xf w of 
		    Right xv -> case y xv of
                                  IO yv -> yv w
	            Left err -> Left err -- Changing type 
    x >>  y  = IO (primBind2 x y)
	       where 
		primBind2 (IO xf) y w =
		  case xf w of 
		    Right xv -> case y of
                                  IO yv -> yv w
	            Left err -> Left err -- Changing type 
    return a = IO (primReturn a)
               where
                 primReturn a w = Right a
    fail s   = IO (\w -> Left (IOErrorUser s))


{-
primBind1       x y good bad    = case x    of IO xf -> xf (primBind1' y good bad) bad
primBind1'        y good bad xv = primBind1'' (y xv) good bad
primBind1'' (IO yf) good bad    = yf good bad

primBind2       x y good bad    = case x    of IO xf -> xf (primBind2' y good bad) bad
primBind2'        y good bad _  = primBind2'' y good bad
primBind2'' (IO yf) good bad    = yf good bad

primReturn a  good bad = good a
-}

