module Prelude where

import _FromEnum
import _EnumFromToInc
import _EnumFromToDec

_enumFromThenTo :: a -> a -> Int -> [a]
_enumFromThenTo n n' m =
   let step = _fromEnum n' - _fromEnum n
   in if step >= 0 then
	  _enumFromToInc (_fromEnum n) step m
      else
	  _enumFromToDec (_fromEnum n) step m

