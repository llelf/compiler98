module PackedString(
    PackedString
   ,primComparePS
   ,unpackPS
   ,packString
   ,index
   ,length
   ) where

import Prelude hiding(Ix(range,index,inRange),length)
import FFIBuiltin(PackedString)
import PrimComparePS
import PrimUnpackPS
import PrimPackString
import PrimIndex
import PrimLength
