{-
Initialises tracer and starts traced version of main.
-}
module _Driver where

import PreludeBuiltin
import DIO
import System
import Main
--import DPrelude -- (SR,mkNoSR, Trace(..), R(..), con0, ap1, ap2, initializeDebugger)
--import ExitWith

{-
_toplevel = World

_driver :: World -> IO ()
_driver w =
  case (main >> exitWith ExitSuccess) of 
    IO wf ->
     case wf _toplevel of
	Left err -> error  (show (err :: IOError))
	Right _  -> error "Default exit never done!"
-}

_toplevel = con0 SR Root World NTDummy

_driver :: () -> R ()
_driver () = 
  initializeDebugger (
    case ap2 mkNoSR mkTRoot ((>>) mkNoSR mkTRoot) 
           (main mkNoSR mkTRoot)
           (ap1 mkNoSR mkTRoot 
	     (exitWith mkNoSR mkTRoot)
	     (con0 mkNoSR mkTRoot ExitSuccess mkNTDummy)) of
      R (IO (R f t)) _ -> 
	case f t (con0 mkNoSR mkTRoot World mkNTDummy) of
	  R (Left err) _ -> 
            ap1 mkNoSR mkTRoot (error mkNoSR mkTRoot) 
	      (ap1 mkNoSR mkTRoot (show mkNoSR mkTRoot) 
		(err :: R IOError))
          R (Right (R _ t)) _ ->  con0 mkNoSR t () mkNTDummy)
-- ap1 SR Root (error SR Root) (stringConst SR Root "Default exit never done!")



