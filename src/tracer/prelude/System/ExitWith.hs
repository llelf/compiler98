module System where

import System
import DIO
import LowSystem(primExitWith)


exitWith :: ExitCode -> IO a
exitWith code = primExitWith code

{-
mycExitWith primitive 2 :: Trace -> ExitCode -> ()

myexitWith :: SR -> Trace -> R (Trace -> R ExitCode -> R (IO ()))
myexitWith sr t = fun1 NTDummy exitWith' sr t
    where exitWith' t (R c _) = case mycExitWith t c of
                                    () -> R (IO (R (cont t) t)) t
	  cont t x y = R (Right (R () t)) t
-}
