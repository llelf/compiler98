module Error where

import List
import IO
import System

exit = exitWith (ExitFailure (-1))

can'tOpen filename ioError =
  do
    hPutStr stderr ("Can't open "++filename ++ "\n")
    exit

errorStr filename msg = "In file "++filename++":\n"++msg ++ "\n"

can'tOpenStr name [filename] ioerror =
   "Can't open "++ filename  ++ " when trying to read "++name++".\n"
can'tOpenStr name filename ioerror =
   "Can't open any of:\n "++ concatMap (++"\n ") (nub filename)
   ++ "when trying to read "++name++".\n"

errorMsg filename msg =
  do
    hPutStr stderr  (errorStr filename msg)
    exit

can'tOpenAnyOf name filename ioError =
  do
    hPutStr stderr (can'tOpenStr name filename ioError)
    exit

errorLC  l c msg =
  error ("Error: line "++show l ++", column " ++ show c ++ ": " ++ msg++"\n")

