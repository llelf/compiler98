module NonStdTrace(trace) where

import PrimHPutChar(cHPutChar)
import PreludeBuiltin(Handle, stdout)
import DIO

trace :: String -> a -> a
trace s a = f s
    where f [] = a
          f (c:cs) = case cHPutChar stdout c of
	                      Left _ -> f cs
	                      Right _ -> f cs
