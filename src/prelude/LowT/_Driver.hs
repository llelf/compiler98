module _Driver where

import PreludeBuiltin
import DIO
import System
import Main
--import DPrelude -- (SR(..), Trace(..), R(..), con0, ap1, ap2, initializeDebugger)
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
           case ap2 SR Root ((>>) SR Root) (main SR Root)
                                         (ap1 SR Root 
					    (exitWith SR Root)
				            (con0 SR Root ExitSuccess NTDummy)) of
	     R (IO (R f t)) _ -> 
	      case f t (con0 SR Root World NTDummy) of
	          R (Left err) _ -> ap1 SR Root (error SR Root) 
		                                (ap1 SR Root (show SR Root) 
						             (err :: R IOError))
		  R (Right (R _ t)) _ ->  con0 SR t () NTDummy)
		  -- ap1 SR Root (error SR Root) (stringConst SR Root "Default exit never done!")

